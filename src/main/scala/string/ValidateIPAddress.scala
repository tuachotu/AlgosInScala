package string

//A valid IPv4 address is an IP in the form "x1.x2.x3.x4"
// where 0 <= xi <= 255
// and xi cannot contain leading zeros.
// valid =  "192.168.1.1" and "192.168.1.0"
// invalid = "192.168.01.1" , "192.168.1.00", "192.168@1.1"
object ValidateIPAddress  extends  App{

  def valiateIPAdress(ipAddress: String): Boolean = {
    ipAddress match {
      case _ if ipAddress.isEmpty => false
      case _ if ipAddress.split('.').length != 4 => false
      case _  =>
        val components = ipAddress.split('.')
        // Check if all components made of numbers, doesnt start with
        if (components.forall(num => num.forall(_.isDigit)) ) {
          val result = components.foldLeft(true) { ( resultTillNow, componentsToCheck) =>
            if (resultTillNow) {
              if (componentsToCheck.isEmpty) false
              else if (componentsToCheck.length > 1 && componentsToCheck.startsWith("0"))  false
              else if (componentsToCheck.toInt <= 255) true
              else false
            } else false
          }
          result
        }  else {
        false
        }
    }

  }

  def valiateIPAdressSmart(ipAddress: String): Boolean = {
    val components = ipAddress.split('.')
    components match {
      case _ if components.length != 4 => false
      case _ =>
        components.forall { component =>
          component.nonEmpty && component.forall(_.isDigit) && {
            if (component.length > 1 && component.startsWith("0")) false else true
          } && (component.toInt < 256)
        }
    }
  }

  List("192.168.1.1", "192.168.1.0", "192.168.01.1" , "192.168.1.00", "192.168@1.1") foreach { ip =>
    println(ip , valiateIPAdressSmart(ip))
  }

//  List( "192.168.01.1") foreach { ip =>
//    println(ip, valiateIPAdress(ip))
//  }

}
