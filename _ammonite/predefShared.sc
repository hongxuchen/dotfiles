import java.security.MessageDigest

def md5(text: String): String = MessageDigest.getInstance("MD5").digest(text.getBytes).map(c => "%02x".format(0xFF & c)).foldLeft("")(_ + _)
