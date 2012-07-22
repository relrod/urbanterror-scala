import java.net.{DatagramPacket, DatagramSocket}

class UrbanTerror(server: String, rconPassword: String, port: Int = 27960) {

  case class Player(name: String, ping: Int, score: Int)

  private val socket = new DatagramSocket

  /** Send a command to the server via UDP.
    *
    * @param command The command being sent to the server.
    * @return A String containing the response.
    */
  def sendCommand(command: String): String = {
    val magic = Array(0xff, 0xff, 0xff, 0xff).map(_.toByte)
    val rawCommand = magic ++ command.getBytes ++ "\n".getBytes

    val packet = new DatagramPacket(
      rawCommand,
      rawCommand.length,
      java.net.InetAddress.getByName(server),
      port)
    socket.send(packet)

    val buffer = Array.ofDim[Byte](2048)
    val receivedPacket = new DatagramPacket(buffer, buffer.length)
    socket.receive(receivedPacket)
    new String(receivedPacket.getData())
  }

  /** Send an rcon command to the server.
    *
    * @param command The command to send with rcon
    * @return A String containing the result of the call to sendCommand().
    */
  def rcon(command: String) =
    sendCommand("rcon %s %s".format(rconPassword, command))

  /** Get the players connected to the server.
    *
    * @return List[Player] containing each player connected to the server.
    */
  def players(): List[Player] = {
    val playerList = sendCommand("getstatus")
                     .lines
                     .toList
                     .drop(2)
                     .init
                     .map(_.split(" "))
    (for (player <- playerList) yield Player(player(2).tail.init,
                                             player(1) toInt,
                                             player(0) toInt)) toList
  }

  /** Get the current settings of the server.
    *
    * @return A Map[String, String] of settings to their values.
    */
  def settings(): Map[String, String] = {
    var settingsList = sendCommand("getstatus")
                       .lines
                       .toList(1)
                       .split("""\\""")
                       .filter(_ != "")
    Map() ++ (settingsList.grouped(2).toList.map (o => (o(0), o(1))))
  }
}

// NOT PART OF THE LIBRARY ITSELF.
// TODO: Use ScalaTest instead of harcoding tests in main(). :P
object UrbanTerror {
  
  val MaxGear = 63
  val GearTypes = Map(
    "knives"   -> 0,
    "grenades" -> 1,
    "snipers"  -> 2,
    "spas"     -> 4,
    "pistols"  -> 8,
    "autos"    -> 16,
    "negev"    -> 32
  )
  val GameModes = Map(
    0 -> "Free For All",
    3 -> "Team Death Match",
    4 -> "Team Survivor",
    5 -> "Follow the Leader",
    6 -> "Capture and Hold",
    7 -> "Capture the Flag",
    8 -> "Bomb Mode"
  )

  def main(args: Array[String]) = {
    val ut = args.length match {
      case 0 => {
        println("Usage: UrbanTerror SERVERNAME [rcon password]")
        System.exit(1)
        throw new Exception
      }
      case 1 => new UrbanTerror(args(0), "")
      case 2 => new UrbanTerror(args(0), args(1))
    }
    println(ut.players())
  }

  
  /** Convert gear to their Urban Terror config number.
    *
    * @throws IllegalArgumentException if given an invalid string of gear.
    * @return An Int - the Urban Terror config number for the given gear.
    */
  def gear(theGear: List[String]): Int = {
    theGear.foreach { weapon =>
      if (!GearTypes.keys.toList.contains(weapon))
        throw new IllegalArgumentException("%s is not a valid gear type."
                                           .format(weapon))
    }
    MaxGear - theGear.map(GearTypes(_)).sum
  }
}
