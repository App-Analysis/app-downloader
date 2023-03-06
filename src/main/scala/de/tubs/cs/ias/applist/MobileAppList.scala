package de.tubs.cs.ias.applist

case class MobileApp(name: String,
                     bundleId: String,
                     category: String,
                     rank: Int,
                     price: String)

case class MobileAppList(apps: List[MobileApp], name: String)
