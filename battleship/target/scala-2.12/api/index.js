Index.PACKAGES = {"battleship" : [{"name" : "battleship.Board", "shortDescription" : "The board of the game: 10x10 cells", "members_case class" : [{"label" : "updateCellState", "tail" : "(x: Int, y: Int, gridToUpdate: List[List[CellType]], newState: CellType.Value): List[List[CellType]]", "member" : "battleship.Board.updateCellState", "link" : "battleship\/Board.html#updateCellState(x:Int,y:Int,gridToUpdate:List[List[battleship.CellType.CellType]],newState:battleship.CellType.Value):List[List[battleship.CellType.CellType]]", "kind" : "def"}, {"label" : "getCellState", "tail" : "(x: Int, y: Int): CellType", "member" : "battleship.Board.getCellState", "link" : "battleship\/Board.html#getCellState(x:Int,y:Int):battleship.CellType.CellType", "kind" : "def"}, {"label" : "canPlaceShip", "tail" : "(ship: Ship): Boolean", "member" : "battleship.Board.canPlaceShip", "link" : "battleship\/Board.html#canPlaceShip(ship:battleship.Ship):Boolean", "kind" : "def"}, {"label" : "canPlaceShipOnPosition", "tail" : "(x: Int, y: Int): Boolean", "member" : "battleship.Board.canPlaceShipOnPosition", "link" : "battleship\/Board.html#canPlaceShipOnPosition(x:Int,y:Int):Boolean", "kind" : "def"}, {"label" : "positionIsShot", "tail" : "(x: Int, y: Int): Boolean", "member" : "battleship.Board.positionIsShot", "link" : "battleship\/Board.html#positionIsShot(x:Int,y:Int):Boolean", "kind" : "def"}, {"label" : "isPositionValid", "tail" : "(x: Int, y: Int): Boolean", "member" : "battleship.Board.isPositionValid", "link" : "battleship\/Board.html#isPositionValid(x:Int,y:Int):Boolean", "kind" : "def"}, {"label" : "placeShip", "tail" : "(ship: Ship): Board", "member" : "battleship.Board.placeShip", "link" : "battleship\/Board.html#placeShip(ship:battleship.Ship):battleship.Board", "kind" : "def"}, {"member" : "battleship.Board#<init>", "error" : "unsupported entity"}, {"label" : "grid", "tail" : ": List[List[CellType]]", "member" : "battleship.Board.grid", "link" : "battleship\/Board.html#grid:List[List[battleship.CellType.CellType]]", "kind" : "val"}, {"label" : "synchronized", "tail" : "(arg0: ⇒ T0): T0", "member" : "scala.AnyRef.synchronized", "link" : "battleship\/Board.html#synchronized[T0](x$1:=>T0):T0", "kind" : "final def"}, {"label" : "##", "tail" : "(): Int", "member" : "scala.AnyRef.##", "link" : "battleship\/Board.html###():Int", "kind" : "final def"}, {"label" : "!=", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.!=", "link" : "battleship\/Board.html#!=(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "==", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.==", "link" : "battleship\/Board.html#==(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "ne", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.ne", "link" : "battleship\/Board.html#ne(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "eq", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.eq", "link" : "battleship\/Board.html#eq(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "finalize", "tail" : "(): Unit", "member" : "scala.AnyRef.finalize", "link" : "battleship\/Board.html#finalize():Unit", "kind" : "def"}, {"label" : "wait", "tail" : "(): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Board.html#wait():Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long, arg1: Int): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Board.html#wait(x$1:Long,x$2:Int):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Board.html#wait(x$1:Long):Unit", "kind" : "final def"}, {"label" : "notifyAll", "tail" : "(): Unit", "member" : "scala.AnyRef.notifyAll", "link" : "battleship\/Board.html#notifyAll():Unit", "kind" : "final def"}, {"label" : "notify", "tail" : "(): Unit", "member" : "scala.AnyRef.notify", "link" : "battleship\/Board.html#notify():Unit", "kind" : "final def"}, {"label" : "clone", "tail" : "(): AnyRef", "member" : "scala.AnyRef.clone", "link" : "battleship\/Board.html#clone():Object", "kind" : "def"}, {"label" : "getClass", "tail" : "(): Class[_]", "member" : "scala.AnyRef.getClass", "link" : "battleship\/Board.html#getClass():Class[_]", "kind" : "final def"}, {"label" : "asInstanceOf", "tail" : "(): T0", "member" : "scala.Any.asInstanceOf", "link" : "battleship\/Board.html#asInstanceOf[T0]:T0", "kind" : "final def"}, {"label" : "isInstanceOf", "tail" : "(): Boolean", "member" : "scala.Any.isInstanceOf", "link" : "battleship\/Board.html#isInstanceOf[T0]:Boolean", "kind" : "final def"}], "case class" : "battleship\/Board.html", "kind" : "case class"}, {"name" : "battleship.CellType", "shortDescription" : "", "object" : "battleship\/CellType$.html", "members_object" : [{"label" : "Val", "tail" : "", "member" : "scala.Enumeration.Val", "link" : "battleship\/CellType$.html#ValextendsEnumeration.this.ValuewithSerializable", "kind" : "class"}, {"label" : "ValueSet", "tail" : "", "member" : "scala.Enumeration.ValueSet", "link" : "battleship\/CellType$.html#ValueSetextendsAbstractSet[Enumeration.this.Value]withSortedSet[Enumeration.this.Value]withSortedSetLike[Enumeration.this.Value,Enumeration.this.ValueSet]withSerializable", "kind" : "class"}, {"label" : "Value", "tail" : "", "member" : "scala.Enumeration.Value", "link" : "battleship\/CellType$.html#ValueextendsOrdered[Enumeration.this.Value]withSerializable", "kind" : "abstract class"}, {"label" : "HIT", "tail" : ": Value", "member" : "battleship.CellType.HIT", "link" : "battleship\/CellType$.html#HIT:battleship.CellType.Value", "kind" : "val"}, {"label" : "MISS", "tail" : ": Value", "member" : "battleship.CellType.MISS", "link" : "battleship\/CellType$.html#MISS:battleship.CellType.Value", "kind" : "val"}, {"label" : "SHIP", "tail" : ": Value", "member" : "battleship.CellType.SHIP", "link" : "battleship\/CellType$.html#SHIP:battleship.CellType.Value", "kind" : "val"}, {"label" : "WATER", "tail" : ": Value", "member" : "battleship.CellType.WATER", "link" : "battleship\/CellType$.html#WATER:battleship.CellType.Value", "kind" : "val"}, {"label" : "CellType", "tail" : "", "member" : "battleship.CellType.CellType", "link" : "battleship\/CellType$.html#CellType=battleship.CellType.Value", "kind" : "type"}, {"label" : "Value", "tail" : "(i: Int, name: String): Value", "member" : "scala.Enumeration.Value", "link" : "battleship\/CellType$.html#Value(i:Int,name:String):Enumeration.this.Value", "kind" : "final def"}, {"label" : "Value", "tail" : "(name: String): Value", "member" : "scala.Enumeration.Value", "link" : "battleship\/CellType$.html#Value(name:String):Enumeration.this.Value", "kind" : "final def"}, {"label" : "Value", "tail" : "(i: Int): Value", "member" : "scala.Enumeration.Value", "link" : "battleship\/CellType$.html#Value(i:Int):Enumeration.this.Value", "kind" : "final def"}, {"label" : "Value", "tail" : "(): Value", "member" : "scala.Enumeration.Value", "link" : "battleship\/CellType$.html#Value:Enumeration.this.Value", "kind" : "final def"}, {"label" : "withName", "tail" : "(s: String): Value", "member" : "scala.Enumeration.withName", "link" : "battleship\/CellType$.html#withName(s:String):Enumeration.this.Value", "kind" : "final def"}, {"label" : "apply", "tail" : "(x: Int): Value", "member" : "scala.Enumeration.apply", "link" : "battleship\/CellType$.html#apply(x:Int):Enumeration.this.Value", "kind" : "final def"}, {"label" : "maxId", "tail" : "(): Int", "member" : "scala.Enumeration.maxId", "link" : "battleship\/CellType$.html#maxId:Int", "kind" : "final def"}, {"label" : "nextName", "tail" : ": Iterator[String]", "member" : "scala.Enumeration.nextName", "link" : "battleship\/CellType$.html#nextName:Iterator[String]", "kind" : "var"}, {"label" : "nextId", "tail" : ": Int", "member" : "scala.Enumeration.nextId", "link" : "battleship\/CellType$.html#nextId:Int", "kind" : "var"}, {"label" : "values", "tail" : "(): ValueSet", "member" : "scala.Enumeration.values", "link" : "battleship\/CellType$.html#values:Enumeration.this.ValueSet", "kind" : "def"}, {"label" : "toString", "tail" : "(): String", "member" : "scala.Enumeration.toString", "link" : "battleship\/CellType$.html#toString():String", "kind" : "def"}, {"label" : "readResolve", "tail" : "(): AnyRef", "member" : "scala.Enumeration.readResolve", "link" : "battleship\/CellType$.html#readResolve():AnyRef", "kind" : "def"}, {"label" : "synchronized", "tail" : "(arg0: ⇒ T0): T0", "member" : "scala.AnyRef.synchronized", "link" : "battleship\/CellType$.html#synchronized[T0](x$1:=>T0):T0", "kind" : "final def"}, {"label" : "##", "tail" : "(): Int", "member" : "scala.AnyRef.##", "link" : "battleship\/CellType$.html###():Int", "kind" : "final def"}, {"label" : "!=", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.!=", "link" : "battleship\/CellType$.html#!=(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "==", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.==", "link" : "battleship\/CellType$.html#==(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "ne", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.ne", "link" : "battleship\/CellType$.html#ne(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "eq", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.eq", "link" : "battleship\/CellType$.html#eq(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "finalize", "tail" : "(): Unit", "member" : "scala.AnyRef.finalize", "link" : "battleship\/CellType$.html#finalize():Unit", "kind" : "def"}, {"label" : "wait", "tail" : "(): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/CellType$.html#wait():Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long, arg1: Int): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/CellType$.html#wait(x$1:Long,x$2:Int):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/CellType$.html#wait(x$1:Long):Unit", "kind" : "final def"}, {"label" : "notifyAll", "tail" : "(): Unit", "member" : "scala.AnyRef.notifyAll", "link" : "battleship\/CellType$.html#notifyAll():Unit", "kind" : "final def"}, {"label" : "notify", "tail" : "(): Unit", "member" : "scala.AnyRef.notify", "link" : "battleship\/CellType$.html#notify():Unit", "kind" : "final def"}, {"label" : "clone", "tail" : "(): AnyRef", "member" : "scala.AnyRef.clone", "link" : "battleship\/CellType$.html#clone():Object", "kind" : "def"}, {"label" : "equals", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.equals", "link" : "battleship\/CellType$.html#equals(x$1:Any):Boolean", "kind" : "def"}, {"label" : "hashCode", "tail" : "(): Int", "member" : "scala.AnyRef.hashCode", "link" : "battleship\/CellType$.html#hashCode():Int", "kind" : "def"}, {"label" : "getClass", "tail" : "(): Class[_]", "member" : "scala.AnyRef.getClass", "link" : "battleship\/CellType$.html#getClass():Class[_]", "kind" : "final def"}, {"label" : "asInstanceOf", "tail" : "(): T0", "member" : "scala.Any.asInstanceOf", "link" : "battleship\/CellType$.html#asInstanceOf[T0]:T0", "kind" : "final def"}, {"label" : "isInstanceOf", "tail" : "(): Boolean", "member" : "scala.Any.isInstanceOf", "link" : "battleship\/CellType$.html#isInstanceOf[T0]:Boolean", "kind" : "final def"}], "kind" : "object"}, {"name" : "battleship.Game", "shortDescription" : "Entry point of the application (main)", "object" : "battleship\/Game$.html", "members_object" : [{"label" : "aiPlaceShips", "tail" : "(shipsToPlace: Map[String, Int], player: Player): Player", "member" : "battleship.Game.aiPlaceShips", "link" : "battleship\/Game$.html#aiPlaceShips(shipsToPlace:scala.collection.immutable.Map[String,Int],player:battleship.Player):battleship.Player", "kind" : "def"}, {"label" : "userPlaceShips", "tail" : "(shipsToPlace: Map[String, Int], player: Player): Player", "member" : "battleship.Game.userPlaceShips", "link" : "battleship\/Game$.html#userPlaceShips(shipsToPlace:scala.collection.immutable.Map[String,Int],player:battleship.Player):battleship.Player", "kind" : "def"}, {"label" : "createAI", "tail" : "(aiName: String): Player", "member" : "battleship.Game.createAI", "link" : "battleship\/Game$.html#createAI(aiName:String):battleship.Player", "kind" : "def"}, {"label" : "createPlayer", "tail" : "(num: Int): Player", "member" : "battleship.Game.createPlayer", "link" : "battleship\/Game$.html#createPlayer(num:Int):battleship.Player", "kind" : "def"}, {"label" : "resetPlayer", "tail" : "(player: Player): Player", "member" : "battleship.Game.resetPlayer", "link" : "battleship\/Game$.html#resetPlayer(player:battleship.Player):battleship.Player", "kind" : "def"}, {"label" : "mainLoopAIvsAI", "tail" : "(gameState: GameState, numberOfGamesToPlay: Int, currentGameNumber: Int): GameState", "member" : "battleship.Game.mainLoopAIvsAI", "link" : "battleship\/Game$.html#mainLoopAIvsAI(gameState:battleship.GameState,numberOfGamesToPlay:Int,currentGameNumber:Int):battleship.GameState", "kind" : "def"}, {"label" : "mainLoop", "tail" : "(gameState: GameState): GameState", "member" : "battleship.Game.mainLoop", "link" : "battleship\/Game$.html#mainLoop(gameState:battleship.GameState):battleship.GameState", "kind" : "def"}, {"label" : "chooseMode", "tail" : "(): Unit", "member" : "battleship.Game.chooseMode", "link" : "battleship\/Game$.html#chooseMode():Unit", "kind" : "def"}, {"label" : "ships", "tail" : ": Map[String, Int]", "member" : "battleship.Game.ships", "link" : "battleship\/Game$.html#ships:scala.collection.immutable.Map[String,Int]", "kind" : "val"}, {"label" : "main", "tail" : "(args: Array[String]): Unit", "member" : "scala.App.main", "link" : "battleship\/Game$.html#main(args:Array[String]):Unit", "kind" : "def"}, {"label" : "delayedInit", "tail" : "(body: ⇒ Unit): Unit", "member" : "scala.App.delayedInit", "link" : "battleship\/Game$.html#delayedInit(body:=>Unit):Unit", "kind" : "def"}, {"label" : "args", "tail" : "(): Array[String]", "member" : "scala.App.args", "link" : "battleship\/Game$.html#args:Array[String]", "kind" : "def"}, {"label" : "executionStart", "tail" : ": Long", "member" : "scala.App.executionStart", "link" : "battleship\/Game$.html#executionStart:Long", "kind" : "val"}, {"label" : "synchronized", "tail" : "(arg0: ⇒ T0): T0", "member" : "scala.AnyRef.synchronized", "link" : "battleship\/Game$.html#synchronized[T0](x$1:=>T0):T0", "kind" : "final def"}, {"label" : "##", "tail" : "(): Int", "member" : "scala.AnyRef.##", "link" : "battleship\/Game$.html###():Int", "kind" : "final def"}, {"label" : "!=", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.!=", "link" : "battleship\/Game$.html#!=(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "==", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.==", "link" : "battleship\/Game$.html#==(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "ne", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.ne", "link" : "battleship\/Game$.html#ne(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "eq", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.eq", "link" : "battleship\/Game$.html#eq(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "finalize", "tail" : "(): Unit", "member" : "scala.AnyRef.finalize", "link" : "battleship\/Game$.html#finalize():Unit", "kind" : "def"}, {"label" : "wait", "tail" : "(): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Game$.html#wait():Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long, arg1: Int): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Game$.html#wait(x$1:Long,x$2:Int):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Game$.html#wait(x$1:Long):Unit", "kind" : "final def"}, {"label" : "notifyAll", "tail" : "(): Unit", "member" : "scala.AnyRef.notifyAll", "link" : "battleship\/Game$.html#notifyAll():Unit", "kind" : "final def"}, {"label" : "notify", "tail" : "(): Unit", "member" : "scala.AnyRef.notify", "link" : "battleship\/Game$.html#notify():Unit", "kind" : "final def"}, {"label" : "toString", "tail" : "(): String", "member" : "scala.AnyRef.toString", "link" : "battleship\/Game$.html#toString():String", "kind" : "def"}, {"label" : "clone", "tail" : "(): AnyRef", "member" : "scala.AnyRef.clone", "link" : "battleship\/Game$.html#clone():Object", "kind" : "def"}, {"label" : "equals", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.equals", "link" : "battleship\/Game$.html#equals(x$1:Any):Boolean", "kind" : "def"}, {"label" : "hashCode", "tail" : "(): Int", "member" : "scala.AnyRef.hashCode", "link" : "battleship\/Game$.html#hashCode():Int", "kind" : "def"}, {"label" : "getClass", "tail" : "(): Class[_]", "member" : "scala.AnyRef.getClass", "link" : "battleship\/Game$.html#getClass():Class[_]", "kind" : "final def"}, {"label" : "asInstanceOf", "tail" : "(): T0", "member" : "scala.Any.asInstanceOf", "link" : "battleship\/Game$.html#asInstanceOf[T0]:T0", "kind" : "final def"}, {"label" : "isInstanceOf", "tail" : "(): Boolean", "member" : "scala.Any.isInstanceOf", "link" : "battleship\/Game$.html#isInstanceOf[T0]:Boolean", "kind" : "final def"}], "kind" : "object"}, {"name" : "battleship.GameState", "shortDescription" : "The current state of the game:- players grids- players fleet- players scores", "members_case class" : [{"label" : "switchPlayers", "tail" : "(): GameState", "member" : "battleship.GameState.switchPlayers", "link" : "battleship\/GameState.html#switchPlayers:battleship.GameState", "kind" : "def"}, {"member" : "battleship.GameState#<init>", "error" : "unsupported entity"}, {"label" : "firstPlayer", "tail" : ": Player", "member" : "battleship.GameState.firstPlayer", "link" : "battleship\/GameState.html#firstPlayer:battleship.Player", "kind" : "val"}, {"label" : "opponent", "tail" : ": Player", "member" : "battleship.GameState.opponent", "link" : "battleship\/GameState.html#opponent:battleship.Player", "kind" : "val"}, {"label" : "active", "tail" : ": Player", "member" : "battleship.GameState.active", "link" : "battleship\/GameState.html#active:battleship.Player", "kind" : "val"}, {"label" : "synchronized", "tail" : "(arg0: ⇒ T0): T0", "member" : "scala.AnyRef.synchronized", "link" : "battleship\/GameState.html#synchronized[T0](x$1:=>T0):T0", "kind" : "final def"}, {"label" : "##", "tail" : "(): Int", "member" : "scala.AnyRef.##", "link" : "battleship\/GameState.html###():Int", "kind" : "final def"}, {"label" : "!=", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.!=", "link" : "battleship\/GameState.html#!=(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "==", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.==", "link" : "battleship\/GameState.html#==(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "ne", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.ne", "link" : "battleship\/GameState.html#ne(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "eq", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.eq", "link" : "battleship\/GameState.html#eq(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "finalize", "tail" : "(): Unit", "member" : "scala.AnyRef.finalize", "link" : "battleship\/GameState.html#finalize():Unit", "kind" : "def"}, {"label" : "wait", "tail" : "(): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/GameState.html#wait():Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long, arg1: Int): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/GameState.html#wait(x$1:Long,x$2:Int):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/GameState.html#wait(x$1:Long):Unit", "kind" : "final def"}, {"label" : "notifyAll", "tail" : "(): Unit", "member" : "scala.AnyRef.notifyAll", "link" : "battleship\/GameState.html#notifyAll():Unit", "kind" : "final def"}, {"label" : "notify", "tail" : "(): Unit", "member" : "scala.AnyRef.notify", "link" : "battleship\/GameState.html#notify():Unit", "kind" : "final def"}, {"label" : "clone", "tail" : "(): AnyRef", "member" : "scala.AnyRef.clone", "link" : "battleship\/GameState.html#clone():Object", "kind" : "def"}, {"label" : "getClass", "tail" : "(): Class[_]", "member" : "scala.AnyRef.getClass", "link" : "battleship\/GameState.html#getClass():Class[_]", "kind" : "final def"}, {"label" : "asInstanceOf", "tail" : "(): T0", "member" : "scala.Any.asInstanceOf", "link" : "battleship\/GameState.html#asInstanceOf[T0]:T0", "kind" : "final def"}, {"label" : "isInstanceOf", "tail" : "(): Boolean", "member" : "scala.Any.isInstanceOf", "link" : "battleship\/GameState.html#isInstanceOf[T0]:Boolean", "kind" : "final def"}], "case class" : "battleship\/GameState.html", "kind" : "case class"}, {"name" : "battleship.Player", "shortDescription" : "Class to define a player comportement", "members_case class" : [{"label" : "fireAtCell", "tail" : "(x: Int, y: Int, opponent: Player): (Player, Player)", "member" : "battleship.Player.fireAtCell", "link" : "battleship\/Player.html#fireAtCell(x:Int,y:Int,opponent:battleship.Player):(battleship.Player,battleship.Player)", "kind" : "def"}, {"label" : "addHitPosition", "tail" : "(x: Int, y: Int, isSunk: Boolean): List[(Int, Int, Boolean)]", "member" : "battleship.Player.addHitPosition", "link" : "battleship\/Player.html#addHitPosition(x:Int,y:Int,isSunk:Boolean):List[(Int,Int,Boolean)]", "kind" : "def"}, {"label" : "getadjacentPositions", "tail" : "(x: Int, y: Int): List[(Int, Int)]", "member" : "battleship.Player.getadjacentPositions", "link" : "battleship\/Player.html#getadjacentPositions(x:Int,y:Int):List[(Int,Int)]", "kind" : "def"}, {"label" : "checkAdjPos", "tail" : "(lastPosChecked: (Int, Int), diff: (Int, Int)): Option[(Int, Int)]", "member" : "battleship.Player.checkAdjPos", "link" : "battleship\/Player.html#checkAdjPos(lastPosChecked:(Int,Int),diff:(Int,Int)):Option[(Int,Int)]", "kind" : "def"}, {"label" : "getNextHit", "tail" : "(shootPossibilities: List[(Int, Int)], lastHit: (Int, Int, Boolean), secondLastHit: (Int, Int, Boolean)): Option[(Int, Int)]", "member" : "battleship.Player.getNextHit", "link" : "battleship\/Player.html#getNextHit(shootPossibilities:List[(Int,Int)],lastHit:(Int,Int,Boolean),secondLastHit:(Int,Int,Boolean)):Option[(Int,Int)]", "kind" : "def"}, {"label" : "chooseTarget", "tail" : "(aiLevel: String, randomX: Random, randomY: Random): (Int, Int)", "member" : "battleship.Player.chooseTarget", "link" : "battleship\/Player.html#chooseTarget(aiLevel:String,randomX:scala.util.Random,randomY:scala.util.Random):(Int,Int)", "kind" : "def"}, {"label" : "getShipOnCell", "tail" : "(x: Int, y: Int, fleet: List[Ship]): Ship", "member" : "battleship.Player.getShipOnCell", "link" : "battleship\/Player.html#getShipOnCell(x:Int,y:Int,fleet:List[battleship.Ship]):battleship.Ship", "kind" : "def"}, {"label" : "isAlive", "tail" : "(): Boolean", "member" : "battleship.Player.isAlive", "link" : "battleship\/Player.html#isAlive:Boolean", "kind" : "def"}, {"member" : "battleship.Player#<init>", "error" : "unsupported entity"}, {"label" : "score", "tail" : ": Int", "member" : "battleship.Player.score", "link" : "battleship\/Player.html#score:Int", "kind" : "val"}, {"label" : "positionsHit", "tail" : ": List[(Int, Int, Boolean)]", "member" : "battleship.Player.positionsHit", "link" : "battleship\/Player.html#positionsHit:List[(Int,Int,Boolean)]", "kind" : "val"}, {"label" : "fleet", "tail" : ": List[Ship]", "member" : "battleship.Player.fleet", "link" : "battleship\/Player.html#fleet:List[battleship.Ship]", "kind" : "val"}, {"label" : "hitsBoard", "tail" : ": Board", "member" : "battleship.Player.hitsBoard", "link" : "battleship\/Player.html#hitsBoard:battleship.Board", "kind" : "val"}, {"label" : "shipsBoard", "tail" : ": Board", "member" : "battleship.Player.shipsBoard", "link" : "battleship\/Player.html#shipsBoard:battleship.Board", "kind" : "val"}, {"label" : "isHuman", "tail" : ": Boolean", "member" : "battleship.Player.isHuman", "link" : "battleship\/Player.html#isHuman:Boolean", "kind" : "val"}, {"label" : "name", "tail" : ": String", "member" : "battleship.Player.name", "link" : "battleship\/Player.html#name:String", "kind" : "val"}, {"label" : "synchronized", "tail" : "(arg0: ⇒ T0): T0", "member" : "scala.AnyRef.synchronized", "link" : "battleship\/Player.html#synchronized[T0](x$1:=>T0):T0", "kind" : "final def"}, {"label" : "##", "tail" : "(): Int", "member" : "scala.AnyRef.##", "link" : "battleship\/Player.html###():Int", "kind" : "final def"}, {"label" : "!=", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.!=", "link" : "battleship\/Player.html#!=(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "==", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.==", "link" : "battleship\/Player.html#==(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "ne", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.ne", "link" : "battleship\/Player.html#ne(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "eq", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.eq", "link" : "battleship\/Player.html#eq(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "finalize", "tail" : "(): Unit", "member" : "scala.AnyRef.finalize", "link" : "battleship\/Player.html#finalize():Unit", "kind" : "def"}, {"label" : "wait", "tail" : "(): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Player.html#wait():Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long, arg1: Int): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Player.html#wait(x$1:Long,x$2:Int):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Player.html#wait(x$1:Long):Unit", "kind" : "final def"}, {"label" : "notifyAll", "tail" : "(): Unit", "member" : "scala.AnyRef.notifyAll", "link" : "battleship\/Player.html#notifyAll():Unit", "kind" : "final def"}, {"label" : "notify", "tail" : "(): Unit", "member" : "scala.AnyRef.notify", "link" : "battleship\/Player.html#notify():Unit", "kind" : "final def"}, {"label" : "clone", "tail" : "(): AnyRef", "member" : "scala.AnyRef.clone", "link" : "battleship\/Player.html#clone():Object", "kind" : "def"}, {"label" : "getClass", "tail" : "(): Class[_]", "member" : "scala.AnyRef.getClass", "link" : "battleship\/Player.html#getClass():Class[_]", "kind" : "final def"}, {"label" : "asInstanceOf", "tail" : "(): T0", "member" : "scala.Any.asInstanceOf", "link" : "battleship\/Player.html#asInstanceOf[T0]:T0", "kind" : "final def"}, {"label" : "isInstanceOf", "tail" : "(): Boolean", "member" : "scala.Any.isInstanceOf", "link" : "battleship\/Player.html#isInstanceOf[T0]:Boolean", "kind" : "final def"}], "case class" : "battleship\/Player.html", "kind" : "case class"}, {"name" : "battleship.Ship", "shortDescription" : "Represents a ship", "members_case class" : [{"label" : "createPositions", "tail" : "(x: Int, y: Int, positions: List[(Int, Int)]): Ship", "member" : "battleship.Ship.createPositions", "link" : "battleship\/Ship.html#createPositions(x:Int,y:Int,positions:List[(Int,Int)]):battleship.Ship", "kind" : "def"}, {"label" : "isSunk", "tail" : "(): Boolean", "member" : "battleship.Ship.isSunk", "link" : "battleship\/Ship.html#isSunk:Boolean", "kind" : "def"}, {"label" : "addShoot", "tail" : "(x: Int, y: Int): Ship", "member" : "battleship.Ship.addShoot", "link" : "battleship\/Ship.html#addShoot(x:Int,y:Int):battleship.Ship", "kind" : "def"}, {"member" : "battleship.Ship#<init>", "error" : "unsupported entity"}, {"label" : "positionsShot", "tail" : ": Set[(Int, Int)]", "member" : "battleship.Ship.positionsShot", "link" : "battleship\/Ship.html#positionsShot:Set[(Int,Int)]", "kind" : "val"}, {"label" : "positions", "tail" : ": List[(Int, Int)]", "member" : "battleship.Ship.positions", "link" : "battleship\/Ship.html#positions:List[(Int,Int)]", "kind" : "val"}, {"label" : "direction", "tail" : ": String", "member" : "battleship.Ship.direction", "link" : "battleship\/Ship.html#direction:String", "kind" : "val"}, {"label" : "size", "tail" : ": Int", "member" : "battleship.Ship.size", "link" : "battleship\/Ship.html#size:Int", "kind" : "val"}, {"label" : "name", "tail" : ": String", "member" : "battleship.Ship.name", "link" : "battleship\/Ship.html#name:String", "kind" : "val"}, {"label" : "synchronized", "tail" : "(arg0: ⇒ T0): T0", "member" : "scala.AnyRef.synchronized", "link" : "battleship\/Ship.html#synchronized[T0](x$1:=>T0):T0", "kind" : "final def"}, {"label" : "##", "tail" : "(): Int", "member" : "scala.AnyRef.##", "link" : "battleship\/Ship.html###():Int", "kind" : "final def"}, {"label" : "!=", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.!=", "link" : "battleship\/Ship.html#!=(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "==", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.==", "link" : "battleship\/Ship.html#==(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "ne", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.ne", "link" : "battleship\/Ship.html#ne(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "eq", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.eq", "link" : "battleship\/Ship.html#eq(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "finalize", "tail" : "(): Unit", "member" : "scala.AnyRef.finalize", "link" : "battleship\/Ship.html#finalize():Unit", "kind" : "def"}, {"label" : "wait", "tail" : "(): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Ship.html#wait():Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long, arg1: Int): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Ship.html#wait(x$1:Long,x$2:Int):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Ship.html#wait(x$1:Long):Unit", "kind" : "final def"}, {"label" : "notifyAll", "tail" : "(): Unit", "member" : "scala.AnyRef.notifyAll", "link" : "battleship\/Ship.html#notifyAll():Unit", "kind" : "final def"}, {"label" : "notify", "tail" : "(): Unit", "member" : "scala.AnyRef.notify", "link" : "battleship\/Ship.html#notify():Unit", "kind" : "final def"}, {"label" : "clone", "tail" : "(): AnyRef", "member" : "scala.AnyRef.clone", "link" : "battleship\/Ship.html#clone():Object", "kind" : "def"}, {"label" : "getClass", "tail" : "(): Class[_]", "member" : "scala.AnyRef.getClass", "link" : "battleship\/Ship.html#getClass():Class[_]", "kind" : "final def"}, {"label" : "asInstanceOf", "tail" : "(): T0", "member" : "scala.Any.asInstanceOf", "link" : "battleship\/Ship.html#asInstanceOf[T0]:T0", "kind" : "final def"}, {"label" : "isInstanceOf", "tail" : "(): Boolean", "member" : "scala.Any.isInstanceOf", "link" : "battleship\/Ship.html#isInstanceOf[T0]:Boolean", "kind" : "final def"}], "case class" : "battleship\/Ship.html", "kind" : "case class"}, {"name" : "battleship.Utils", "shortDescription" : "Utilities functions such as I\/O and random number generation", "object" : "battleship\/Utils$.html", "members_object" : [{"label" : "writeToCSV", "tail" : "(gameState: GameState): Unit", "member" : "battleship.Utils.writeToCSV", "link" : "battleship\/Utils$.html#writeToCSV(gameState:battleship.GameState):Unit", "kind" : "def"}, {"label" : "cellToString", "tail" : "(cellType: CellType): Unit", "member" : "battleship.Utils.cellToString", "link" : "battleship\/Utils$.html#cellToString(cellType:battleship.CellType.CellType):Unit", "kind" : "def"}, {"label" : "displayBoard", "tail" : "(board: Board): Unit", "member" : "battleship.Utils.displayBoard", "link" : "battleship\/Utils$.html#displayBoard(board:battleship.Board):Unit", "kind" : "def"}, {"label" : "askNumberOfSimulations", "tail" : "(): Int", "member" : "battleship.Utils.askNumberOfSimulations", "link" : "battleship\/Utils$.html#askNumberOfSimulations():Int", "kind" : "def"}, {"label" : "askUserForNewGame", "tail" : "(): Int", "member" : "battleship.Utils.askUserForNewGame", "link" : "battleship\/Utils$.html#askUserForNewGame():Int", "kind" : "def"}, {"label" : "askUserChooseMode", "tail" : "(): Int", "member" : "battleship.Utils.askUserChooseMode", "link" : "battleship\/Utils$.html#askUserChooseMode():Int", "kind" : "def"}, {"label" : "askUserForShipDirection", "tail" : "(): Int", "member" : "battleship.Utils.askUserForShipDirection", "link" : "battleship\/Utils$.html#askUserForShipDirection():Int", "kind" : "def"}, {"label" : "askUserForPosition", "tail" : "(axis: String): Int", "member" : "battleship.Utils.askUserForPosition", "link" : "battleship\/Utils$.html#askUserForPosition(axis:String):Int", "kind" : "def"}, {"label" : "askUserToPlaceShip", "tail" : "(shipName: String, shipSize: Int): Unit", "member" : "battleship.Utils.askUserToPlaceShip", "link" : "battleship\/Utils$.html#askUserToPlaceShip(shipName:String,shipSize:Int):Unit", "kind" : "def"}, {"label" : "askUserToPlaceShips", "tail" : "(userName: String): Unit", "member" : "battleship.Utils.askUserToPlaceShips", "link" : "battleship\/Utils$.html#askUserToPlaceShips(userName:String):Unit", "kind" : "def"}, {"label" : "generateRandomDirection", "tail" : "(random: Random): String", "member" : "battleship.Utils.generateRandomDirection", "link" : "battleship\/Utils$.html#generateRandomDirection(random:scala.util.Random):String", "kind" : "def"}, {"label" : "generateRandomPosition", "tail" : "(randomX: Random, randomY: Random): (Int, Int)", "member" : "battleship.Utils.generateRandomPosition", "link" : "battleship\/Utils$.html#generateRandomPosition(randomX:scala.util.Random,randomY:scala.util.Random):(Int,Int)", "kind" : "def"}, {"label" : "askUserForName", "tail" : "(number: Int): String", "member" : "battleship.Utils.askUserForName", "link" : "battleship\/Utils$.html#askUserForName(number:Int):String", "kind" : "def"}, {"label" : "displayError", "tail" : "(m: String): Unit", "member" : "battleship.Utils.displayError", "link" : "battleship\/Utils$.html#displayError(m:String):Unit", "kind" : "def"}, {"label" : "displayMessage", "tail" : "(m: String): Unit", "member" : "battleship.Utils.displayMessage", "link" : "battleship\/Utils$.html#displayMessage(m:String):Unit", "kind" : "def"}, {"label" : "askUserToContinue", "tail" : "(): String", "member" : "battleship.Utils.askUserToContinue", "link" : "battleship\/Utils$.html#askUserToContinue():String", "kind" : "def"}, {"label" : "getUserInputString", "tail" : "(): String", "member" : "battleship.Utils.getUserInputString", "link" : "battleship\/Utils$.html#getUserInputString():String", "kind" : "def"}, {"label" : "getUserInputInt", "tail" : "(): Int", "member" : "battleship.Utils.getUserInputInt", "link" : "battleship\/Utils$.html#getUserInputInt():Int", "kind" : "def"}, {"label" : "clearConsole", "tail" : "(): Unit", "member" : "battleship.Utils.clearConsole", "link" : "battleship\/Utils$.html#clearConsole():Unit", "kind" : "def"}, {"label" : "initializeConsole", "tail" : "(): Unit", "member" : "battleship.Utils.initializeConsole", "link" : "battleship\/Utils$.html#initializeConsole():Unit", "kind" : "def"}, {"label" : "synchronized", "tail" : "(arg0: ⇒ T0): T0", "member" : "scala.AnyRef.synchronized", "link" : "battleship\/Utils$.html#synchronized[T0](x$1:=>T0):T0", "kind" : "final def"}, {"label" : "##", "tail" : "(): Int", "member" : "scala.AnyRef.##", "link" : "battleship\/Utils$.html###():Int", "kind" : "final def"}, {"label" : "!=", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.!=", "link" : "battleship\/Utils$.html#!=(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "==", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.==", "link" : "battleship\/Utils$.html#==(x$1:Any):Boolean", "kind" : "final def"}, {"label" : "ne", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.ne", "link" : "battleship\/Utils$.html#ne(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "eq", "tail" : "(arg0: AnyRef): Boolean", "member" : "scala.AnyRef.eq", "link" : "battleship\/Utils$.html#eq(x$1:AnyRef):Boolean", "kind" : "final def"}, {"label" : "finalize", "tail" : "(): Unit", "member" : "scala.AnyRef.finalize", "link" : "battleship\/Utils$.html#finalize():Unit", "kind" : "def"}, {"label" : "wait", "tail" : "(): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Utils$.html#wait():Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long, arg1: Int): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Utils$.html#wait(x$1:Long,x$2:Int):Unit", "kind" : "final def"}, {"label" : "wait", "tail" : "(arg0: Long): Unit", "member" : "scala.AnyRef.wait", "link" : "battleship\/Utils$.html#wait(x$1:Long):Unit", "kind" : "final def"}, {"label" : "notifyAll", "tail" : "(): Unit", "member" : "scala.AnyRef.notifyAll", "link" : "battleship\/Utils$.html#notifyAll():Unit", "kind" : "final def"}, {"label" : "notify", "tail" : "(): Unit", "member" : "scala.AnyRef.notify", "link" : "battleship\/Utils$.html#notify():Unit", "kind" : "final def"}, {"label" : "toString", "tail" : "(): String", "member" : "scala.AnyRef.toString", "link" : "battleship\/Utils$.html#toString():String", "kind" : "def"}, {"label" : "clone", "tail" : "(): AnyRef", "member" : "scala.AnyRef.clone", "link" : "battleship\/Utils$.html#clone():Object", "kind" : "def"}, {"label" : "equals", "tail" : "(arg0: Any): Boolean", "member" : "scala.AnyRef.equals", "link" : "battleship\/Utils$.html#equals(x$1:Any):Boolean", "kind" : "def"}, {"label" : "hashCode", "tail" : "(): Int", "member" : "scala.AnyRef.hashCode", "link" : "battleship\/Utils$.html#hashCode():Int", "kind" : "def"}, {"label" : "getClass", "tail" : "(): Class[_]", "member" : "scala.AnyRef.getClass", "link" : "battleship\/Utils$.html#getClass():Class[_]", "kind" : "final def"}, {"label" : "asInstanceOf", "tail" : "(): T0", "member" : "scala.Any.asInstanceOf", "link" : "battleship\/Utils$.html#asInstanceOf[T0]:T0", "kind" : "final def"}, {"label" : "isInstanceOf", "tail" : "(): Boolean", "member" : "scala.Any.isInstanceOf", "link" : "battleship\/Utils$.html#isInstanceOf[T0]:Boolean", "kind" : "final def"}], "kind" : "object"}]};