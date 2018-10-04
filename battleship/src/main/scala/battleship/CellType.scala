package battleship

object CellType extends Enumeration {
    type CellType = Value
    val WATER, SHIP, MISS, HIT = Value
}