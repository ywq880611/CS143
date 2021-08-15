(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class Main inherits IO {
   top : StackNode;

   printStack() : Object{
      let node : StackNode <- top in {
         while(not (isvoid node)) loop{
            out_string(node.getCommand().getChar());
            out_string("\n");
            node <- node.getNext();
         }
         pool;
      }
   };

   pushCommand(command : StackCommand) : StackCommand{
      {
         if (isvoid top) then{
            let nil : StackNode in { -- how long the nil lives?
               top <- (new StackNode).init(command, nil);
            };
         }
         else {
            top <- top.push(command);
         } fi;
         command;
      }
   };

   executeStackMachine(inString: String) : Object {
      {
         if (inString = "+") then
         {
            pushCommand((new PlusCommand).init());
         }
         else
            if (inString = "s") then
               pushCommand((new SwapCommand).init())
            else
               if (inString = "d") then
                  printStack()
               else
                  if (inString = "x") then
                     -- stop
                     {
                        (new IO).out_string("stop!\n");
                        abort();
                     }
                  else
                     if (inString = "e") then
                        let node: StackNode <- top in {
                           if (not (isvoid node)) then
                              top <- node.getCommand().execute(node)
                           else
                              0
                           fi;
                        }
                     else
                        pushCommand((new IntCommand).init((new A2I).a2i(inString)))
                     fi
                  fi
               fi
            fi
         fi;
      }
   };


   main() : Object {
      --out_string("Nothing implemented\n")
      let inString : String in{
         while (true) loop{
            out_string(">");
            inString <- in_string();
            out_string(inString);
            out_string("\n");
            executeStackMachine(inString);
         }
         pool;
      }
   };

};

class StackCommand{
   getChar() : String {
      "Called from base class"
   };

   execute(node : StackNode) : StackNode {
      let ret : StackNode in {
         (new IO).out_string("Undefined execution!\n");
         ret;
      }
   };

   getNumber() : Int{
      0
   };
};


class IntCommand inherits StackCommand{
   number : Int;

   init(num : Int): SELF_TYPE{
      {
         number <- num;
         self;
      }
   };

   execute(node : StackNode) : StackNode{
      node
   };

   getChar(): String{
      (new A2I).i2a(number)
   };

   getNumber(): Int{
      number
   };
};

class PlusCommand inherits StackCommand{

   init(): SELF_TYPE{
      {
         self;
      }
   };

   execute(node: StackNode): StackNode {
      let n1: StackNode <- node.getNext(),
         n2: StackNode <- n1.getNext(),
         sum: Int,
         ret: StackNode in {
            if (not (isvoid n1)) then
               if (not (isvoid n2)) then {
                  sum <- n1.getCommand().getNumber() + n2.getCommand().getNumber();
                  ret <- (new StackNode).init((new IntCommand).init(sum), n2.getNext());
               } 
               else 
                  0
               fi
            else
               0
            fi;
            ret;
         }
   };

   getChar() : String{
      "+"
   };
};

class SwapCommand inherits StackCommand{
   init(): SELF_TYPE{
      self
   };

   execute(node : StackNode) : StackNode{
      let next : StackNode <- node.getNext().getNext() in {
         node <- node.getNext();
         node.setNext(next.getNext());
         next.setNext(node);
         next;
      }
   };

   getChar() : String{
      "s"
   };  
};

class StackNode{
   command : StackCommand;
   next : StackNode;

   init(com : StackCommand, ne : StackNode) : StackNode{
      {
         command <- com;
         next <- ne;
         self;
      }
   };

   push(com : StackCommand) : StackNode{
      let newNode : StackNode in{ -- how long the newNode lives?
         newNode <- (new StackNode).init(com, self);
         newNode;
      }
   };

   getCommand() : StackCommand{
      command
   };

   getNext() : StackNode{
      next
   };

   setNext(node : StackNode) : StackNode{
      next <- node
   };
};