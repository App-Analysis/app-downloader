package de.tubs.cs.ias.apps.ios

object StoreAuthenticationFailure extends Error {
  override def getMessage: String =
    "The store login failed, probably the 2FA code needs an update."
}
