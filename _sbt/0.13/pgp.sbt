import com.typesafe.sbt.pgp.PgpKeys.{pgpSecretRing,pgpPublicRing}
pgpSecretRing := file(Path.userHome.absolutePath + "/tools/private/gpg/hongxuchen_priv.asc")
pgpPublicRing := file(Path.userHome.absolutePath + "/tools/dotfiles/misc/hongxuchen_pub.asc")
