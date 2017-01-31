Debugging Library
=================

debug.ml and a macro file xdebug.cppo contains a list of commands
that are helpful for debugging.

Firstly, you can use the following
proceduresfor tracing, some of them can be selectively turned
on/off. They are turned on with the option -dd. As these
are macro commands, they will also add line number of
program where they were printed.

Tracing commands:
   y_tinfo_hp  (selective trace printing)
   y_binfo_hp (always print)
   y_ninfo_hp  (no printing)

Two examples:
   let () = y_tinfo_hp (add_str "inside binop with " pr_id) op in
   let () = y_tinfo_pp "inside \\ " in

Note that _hp is for higher-order print, _pp for string printing.
An example of traced generated via ./epli e1.epl -dd

Loading ePL program ..
  false = (\true) & false | (2+3 =4)
  as |[&[=[BoolConst(false),\[BoolConst(true)]],BoolConst(false)],=[+[IntConst(2),IntConst(3)],IntConst(4)]]
Type checking program ..
  inferred type : Bool
Evaluating ==>
!!! **ePL_inter.ml#22:inside \ 
!!! **ePL_inter.ml#32:inside ~ with :=
!!! **ePL_inter.ml#32:inside ~ with :&
!!! **ePL_inter.ml#32:inside ~ with :+
!!! **ePL_inter.ml#32:inside ~ with :=
!!! **ePL_inter.ml#32:inside ~ with :|BoolConst(true)

There help us trace the flow of computation.

Secondly, you can add wrappers to trace function calls of the
form (fn arg1 .. argn). These wrappers would be written in
the following format:
  Debug.no_N "..." pr1 .. prN pr_res fn arg1 .. argN

You need to supply printers for the arguments pr1 .. prN and also
printer for the result pr_res. Each invocation of the call will
then be traced. At the call-site, you can also add macros command
that will allow the line number where they were invoked to be
traced, as follows:

   x_add   (add line tracing to debug call, at least 2 para)
   x_add_1 (add line tracing to debug call, at least 1 para)

An example of this is:
   if (x_add type_check arg IntType) then Some IntType

You may invoke call tracing using -dre ".." with a regular expression
on the calls that are being traced. An example is:

./epli e1.epl -dre "type.*"

(==ePL_inter.ml#254==)
type_check@2@1
type_check inp1 :&[=[BoolConst(false),\[BoolConst(true)]],BoolConst(false)]
type_check inp2 :Bool
type_check@2 EXIT:true

(==ePL_inter.ml#254==)
type_check@3@1
type_check inp1 :=[+[IntConst(2),IntConst(3)],IntConst(4)]
type_check inp2 :Bool
type_check@3 EXIT:true

(==ePL_inter.ml#308==)
type_infer@1
type_infer inp1 :|[&[=[BoolConst(false),\[BoolConst(true)]],BoolConst(false)],=[+[IntConst(2),IntConst(3)],IntConst(4)]]
type_infer@1 EXIT:Some(Bool)
  inferred type : Bool
Evaluating ==> BoolConst(true)

If you have written a wrapper for oneStep, you can trace
its call by using

./epli e1.epl -dre "one"

(====)
oneStep@1
oneStep inp1 :|[&[=[BoolConst(false),\[BoolConst(true)]],BoolConst(false)],=[+[IntConst(2),IntConst(3)],IntConst(4)]]
oneStep@1 EXIT:|[&[=[BoolConst(false),BoolConst(false)],BoolConst(false)],=[+[IntConst(2),IntConst(3)],IntConst(4)]]

(====)
oneStep@2
oneStep inp1 :|[&[=[BoolConst(false),BoolConst(false)],BoolConst(false)],=[+[IntConst(2),IntConst(3)],IntConst(4)]]
oneStep@2 EXIT:|[&[BoolConst(true),BoolConst(false)],=[+[IntConst(2),IntConst(3)],IntConst(4)]]

(====)
oneStep@3
oneStep inp1 :|[&[BoolConst(true),BoolConst(false)],=[+[IntConst(2),IntConst(3)],IntConst(4)]]
oneStep@3 EXIT:|[BoolConst(true),=[+[IntConst(2),IntConst(3)],IntConst(4)]]

(====)
oneStep@4
oneStep inp1 :|[BoolConst(true),=[+[IntConst(2),IntConst(3)],IntConst(4)]]
oneStep@4 EXIT:|[BoolConst(true),=[IntConst(5),IntConst(4)]]

(====)
oneStep@5
oneStep inp1 :|[BoolConst(true),=[IntConst(5),IntConst(4)]]
oneStep@5 EXIT:|[BoolConst(true),BoolConst(false)]

If you want to reveal where they came from, please use x_add_1
prior to each oneStep call.
