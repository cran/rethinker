commCodes<-c(makeArray=2,makeObj=3,var=10,javascript=11,uuid=169,http=153,error=12,implicitVar=13,db=14,table=15,get=16,getAll=78,eq=17,ne=18,lt=19,le=20,gt=21,ge=22,not=23,add=24,sub=25,mul=26,div=27,mod=28,floor=183,ceil=184,round=185,append=29,prepend=80,difference=95,setInsert=88,setIntersection=89,setUnion=90,setDifference=91,slice=30,skip=70,limit=71,offsetsOf=87,contains=93,getField=31,keys=94,values=186,object=143,hasFields=32,withFields=96,pluck=33,without=34,merge=35,betweenDeprecated=36,between=182,reduce=37,map=38,fold=187,filter=39,concatMap=40,orderBy=41,distinct=42,count=43,isEmpty=86,union=44,nth=45,bracket=170,innerJoin=48,outerJoin=49,eqJoin=50,zip=72,range=173,insertAt=82,deleteAt=83,changeAt=84,spliceAt=85,coerceTo=51,typeOf=52,update=53,delete=54,replace=55,insert=56,dbCreate=57,dbDrop=58,dbList=59,tableCreate=60,tableDrop=61,tableList=62,config=174,status=175,wait=177,reconfigure=176,rebalance=179,sync=138,grant=188,indexCreate=75,indexDrop=76,indexList=77,indexStatus=139,indexWait=140,indexRename=156,setWriteHook=189,getWriteHook=190,funcall=64,branch=65,or=66,and=67,forEach=68,func=69,asc=73,desc=74,info=79,match=97,upcase=141,downcase=142,sample=81,default=92,json=98,toJsonString=172,iso8601=99,toIso8601=100,epochTime=101,toEpochTime=102,now=103,inTimezone=104,during=105,date=106,timeOfDay=126,timezone=127,year=128,month=129,day=130,dayOfWeek=131,dayOfYear=132,hours=133,minutes=134,seconds=135,time=136,monday=107,tuesday=108,wednesday=109,thursday=110,friday=111,saturday=112,sunday=113,january=114,february=115,march=116,april=117,may=118,june=119,july=120,august=121,september=122,october=123,november=124,december=125,literal=137,group=144,sum=145,avg=146,min=147,max=148,split=149,ungroup=150,random=151,changes=152,args=154,binary=155,geojson=157,toGeojson=158,point=159,line=160,polygon=161,distance=162,intersects=163,includes=164,circle=165,getIntersecting=166,fill=167,getNearest=168,polygonSub=171,minval=180,maxval=181);

countArgs<-function(f)
 length(as.list(args(f)))-1;

setReqlClass<-function(x){
 class(x)<-"reql"; x
}

lockBaseQuery<-function(x){
 x$baseQuery<-x$query;
 return(x)
}

isReql<-function(x)
 inherits(x,"reql");

enc<-function(com)
 return(as.numeric(commCodes[com]))

coerceDatum<-function(datum){
 if(inherits(datum,"call")){
  stop("Datum is a call!")
 }
 if(inherits(datum,"pairlist")){
  stop("Datum is a pairlist!")
 }
 ## Do not process what is already done
 if(isReql(datum)){
  if(is.environment(datum)){
   #Here, we collapse a reql tree into a query;
   ans<-setReqlClass(datum$query);
   # but this means reql tree is useless, so we can trim it to root,
   # in order to make it re-usable (useful when it is r()$var(..))
   datum$query<-datum$baseQuery;
   return(ans);

  }
  return(datum);
 }

 ## Fix JSON-unfriendly stuff
 #Drop functions
 if(is.function(datum))
 stop("Functions can only exist as direct term arguments.")
 #Environments -> lists
 if(is.environment(datum)) datum<-as.list(datum);
 #Factors -> character
 if(is.factor(datum)) datum<-stats::setNames(as.character(datum),names(datum));
 #Named stuff -> lists
 if(!is.null(names(datum))&&!is.list(datum)) datum<-as.list(datum);

 ## Drill down lists
 if(is.list(datum))
  datum<-lapply(datum,coerceDatum);

 ## Return time
 #Named lists -> those shall be objects, just return
 if(is.list(datum)&&!is.null(names(datum)))
  return(setReqlClass(datum));
 #Un-named stuff with length>1 or a list -> makeArray of elements
 if(length(datum)>1||is.list(datum))
  return(setReqlClass(list(enc("makeArray"),datum)));
 #Single-element vector... scalar
 return(setReqlClass(datum));
}

incorporateTerm<-function(argRaw,id,Q){
 #Convert NULL argRaw meaning argless-function into list()
 if(is.null(argRaw)) argRaw<-list()

 ## Coerce arguments ##
 if(length(argRaw)>0) for(e in 1:length(argRaw)){
  #Evalute it now, not before
  thisArg<-eval(argRaw[[e]],envir=parent.frame(2),enclos=parent.frame(3));
  #Functions got executed with reql arguments
  if(inherits(thisArg,"function")){
   countArgs(thisArg)->arity;
   if(arity==0)
    stop("Anonymous functions without parameters are not supported.");
   internalArgs<-lapply(1:arity,function(e) lockBaseQuery(r()$var(e)));
   body<-do.call(thisArg,internalArgs);
   if(is.environment(body))
    body<-setReqlClass(body$query);
   argRaw[[e]]<-setReqlClass(list(
    enc("func"),
    list(
     list(enc("makeArray"),as.list(1:arity)),
      body
     )
    ));
  }else{
   #Static objects get coerced and makeArray-ied when needed
   argRaw[[e]]<-coerceDatum(thisArg);
  }
 }

 #Following operations will finally kill argRaw pairlist structure
 if(!is.null(names(argRaw))){
  #Some options must be extracted
  argRaw[nchar(names(argRaw))>0]->argOpts;
  argRaw[nchar(names(argRaw))==0]->argArgs;
  names(argArgs)<-NULL;
 }else{
  #No options at all
  argArgs<-argRaw;
  argOpts<-NULL;
 }

 if(!is.null(Q$query)) Q$query<-list(Q$query);
 argArgs<-c(Q$query,argArgs);

 if(is.null(argOpts)){
  Q$query<-list(enc(id),argArgs);
 }else{
  Q$query<-list(enc(id),argArgs,argOpts);
 }
}

funGen<-function(id,Q){
 id<-force(id);
 function(...){
  #This will be used to manipulate with R eval order
  argRaw<-match.call(expand.dots=FALSE)$...;
  incorporateTerm(argRaw,id,Q);
  Q;
 }
}


#' ReQL root
#'
#' Creates ReQL root for building a query.
#' @param db DB name; this is optional, and is just a syntax sugar for \code{r()$db(db)}.
#' @param table Table name; this is optional, requires db to be given, and is just a syntax sugar for \code{r()$db(db)$table(table)}
#' @return ReQL root; use \code{$} (or \code{[[]]}) to chain query terms (like \code{r()$db("test")$table("test")}).
#' In general, anonymous attributes are passed as attributes while named as term options.
#' In context of term arguments, named lists are treated as JSON objects (following \code{rjson} package heuristics), unnamed lists and simple vectors as JSON arrays; classes and attributes are ignored.
#' Term options should be called in the snake case form (for instance \code{return_changes} not \code{returnChanges}), as documented for the original Python driver.
#' To finalise, use \code{$run} or \code{$runAsync}.
#' For a comprehensive description of all terms, see RethinkDB API reference; here we give an overview of some:
#' \item{run(connection,...)}{Evaluate the query; the function will block until first response from RethinkDB to this query will be received.
#' May return cursor, an object representing a stream of data on which \code{\link{cursorNext}} and \code{\link{cursorToList}} can be used to extract actual information.
#' \code{...} may be used to specify run options, like \code{profile}, \code{durability} or \code{read_mode}.}
#' \item{runAsync(connection,callback,...)}{Evaluate the query; for each datum received \code{x}, run \code{callback(x)}.
#' Callback should return \code{TRUE} to be re-evaluated on proceeding data; any other response will cause the query to be dropped immediately.
#' This function returns immediately; to ask R to start evaluating async queries, run \code{\link{drainConnection}}.
#' Note that callbacks can be also called while \code{$run()} blocks waiting for other query to execute.}
#' \item{bracket(...)}{Implementation of the JavaScript \code{(...)} and Python \code{[...]} operation.}
#' \item{funcall(function,atts)}{Implementation of the JavaScript \code{.do()}; note that the order of arguments is different.}
#' @note ReQL is implemented as an environment, thus is mutable unlike most R objects.
#' To this end, you can use variables for chaining like this \code{r()->query;} \code{query$db("a");} \code{query$table("b")}; but consequently you can't use variables to make a re-usable stub, i.e., this is invalid: \code{r()->query;} \code{query$db("a")$table("aa")$run(...)} \code{query$db("b")$table("bb")$run(...);}.
#'
#' If you get "trying to apply non-function" error, you likely have misspelled term name or trying to use a non-existent one.
#'
#' To view raw AST (at any depth), use \code{$query}.
#' @author Miron B. Kursa
#' @examples \dontrun{
#' #Connect to the RethinkDB instance
#' cn<-openConnection()
#'
#' #Get document count in some_db's some_table
#' r()$db("some_db")$table("some_table")$count()$run(cn)
#' #...same can be done shorter
#' r("some_db","some_table")$count()$run(cn)
#'
#' #Fetch 5 random docs from some_db's some_table...
#' r("some_db","some_table")$sample(5)$run(cn)->cursor
#' #...and present as a list`
#' cursorToList(cursor)
#'
#' #Insert an element
#' r("some_db","some_table")$insert(list(id="new",a=1:10,b=list(c=1,d=2)))$run(cn)
#'
#' #Close connection
#' close(cn)
#' }
#' @export
r<-function(db,table){
 setReqlClass(new.env())->Q;
 Q$query<-NULL;
 Q$baseQuery<-NULL;

 #Populating reql composing environment with functions
 for(com in names(commCodes))
  Q[[com]]<-funGen(com,Q);
 Q$row<-function(...)
  stop("Implicit var (row) is not supported by rethinker; use an actual anonymous function.")
 Q$run<-function(connection,...)
  return(syncQuery(connection,rjson::toJSON(Q$query),list(...)))
 Q$runAsync<-function(connection,cb,...)
  return(hookQuery(connection,rjson::toJSON(Q$query),cb,list(...)));
 Q$expr<-function(x){
  if(!is.null(Q$query))
   stop("$expr only makes sense as a first term.");
  Q$query<-coerceDatum(x);
  Q
 }

 #Applying db and table
 if(!missing(db)){
  Q<-Q$db(force(db));
  if(!missing(table))
   Q<-Q$table(force(table));
 }else{
  if(!missing(table))
   stop("You can't give table without specifying db.");
 }

 #Done
 Q
}
