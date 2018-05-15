ClearAll[ chi ];
chi[{n_, i_, j_}, a_, k_ ] := z^n Sin[k z]^i Cos[k z]^j Exp[ -a z^2 ];

(*
(* integrateZw = integrate on the z axis, wrapper function *)
ClearAll[integrateZw];
integrateZw[ arg_ ]/;Not[FreeQ[arg,  Plus]]&&Not[SameQ[Expand[arg], arg]]:= integrateZw[ Expand[ arg ] ];
integrateZw[ a_  + b_ ] := integrateZw[ a ]  +  integrateZw[ b ];
integrateZw[ a_ b_ ] /; FreeQ[ a,  z] := a integrateZw[ b ];

(* integrateZ = integrate on the z axis, with memoization *)
ClearAll[integrateZ];
(* integrateZ[ arg_ ] := integrateZ[ arg ] = FullSimplify[Integrate[ arg , {z, -Infinity, Infinity}, Assumptions->intAssump], intAssump]; *)
integrateZ[ arg_ ] := integrateZ[ arg ] = Integrate[ arg , {z, -Infinity, Infinity}, Assumptions->intAssump ];
*)

integrate[ arg_ ] :=integrate[ arg ] = Integrate[ arg , {z, -Infinity, Infinity}, Assumptions->a1>0&&a2>0 &&k1>0&&k2>0];

(* assumptions for integration and simplification *)
intAssump = a1>0&&a2>0&&k1>0&&k2>0;


(* ssp is a symbol representing Sqrt[Pi / ap], so sppId in fact equals 1 *)
sppId = spp(Sqrt[ap] / Sqrt[Pi]);

(* # # # # # # # # # # # # # # # # # # # # # # # # # *)

ClearAll[mkSubstList];
mkSubstList[ arg_List, head_:t, n_:0 ] := Table[ Rule[ arg[[i]] , head[i + n] ] , {i, 1, Length[arg]} ];

ClearAll[mkSubstListRev];
mkSubstListRev[ arg_List, head_:t, n_:0 ] := Table[ Rule[ head[i + n] , arg[[i]] ] , {i, 1, Length[arg]} ];

ClearAll[instances];
instances[ expr_, patt_ ] := DeleteDuplicates[Extract[expr, Position[expr, patt]]];

(* # # # # # # # # # # # # # # # # # # # # # # # # # *)

(* initialization: these global lists will be filled later *)
powSubst = {};
expSubst = {};
powSubstRev = {};
expSubstRev = {};

(* # # # # # # # # # # # # # # # # # # # # # # # # # *)

(* toSFF = toStringFortranForm *)
toSFF[ arg_ ] := StringReplace[
    ToString[ FortranForm[   arg   ],
        PageWidth->70, 
        FormatType -> InputForm ],
    {"\\\n" -> "&\n       & ",
     "\n "  -> "&\n       & "} ];

(* - - - - - getFortranCode[] - - - - - *)
ClearAll[getFortranCode];
getFortranCode[ arg_,  prefix_:"" ] := Module[{expr,  plist,  elist, code, i,  vname,  val},
                                              (*   (* DEBUG *) ppp = arg;
                                               (* DEBUG *) Save["gfC_arg", ppp]; *)
                                              
	expr = Expand[arg] /. Exp[aaa_]:>exp[ Simplify[aaa] ];
	expr = Collect[sppId expr /. a1 -> ap  -  a2,  exp[_], Simplify];
	expr = Simplify[expr/. powSubst];
	plist = instances[ expr , p[_] ];
	expr = Simplify[expr/. expSubst];
	elist = instances[ expr , e[_] ];
	code = "";
	(* Produce definitions of level 1 auxiliary variables *)
	For[i  =  1, i<=Length[plist], i++,  
		vname = "p"<>ToString[plist[[i,  1]]];
		val = plist[[i]] /. powSubstRev;
		val = ToString[FortranForm[ val ]];
		code = code<>prefix<>vname<>" = "<>val<>"\n";
		];
	(* Produce definitions of level 2 auxiliary variables *)
	For[i  =  1, i<=Length[elist], i++,  
		vname = "e"<>ToString[elist[[i,  1]]];
		val = Block[{p  =  ToExpression["p"<>ToString[#]]&},  
			(elist[[i]] /. expSubstRev) /. Exp[aaa_] -> exp[aaa]
			];
		val = ToString[FortranForm[ val ]];
		code = code<>prefix<>vname<>" = "<>val<>"\n";
		];
	(* Produce definition of the main function *)
	val = Block[{
			p  =  ToExpression["p"<>ToString[#]]&, 
			e  =  ToExpression["e"<>ToString[#]]&
			},  
		expr ];
        code = code<>prefix<>"ans = ";
        code = code<>toSFF[ val ];
        code = code<>"\n";
	Return[code];
];
(* END - - - - - getFortranCode[] - - - - - END *)

(* # # # # # # # # # # # # # # # # # # # # # # # # # *)

getKey[{n1_, i1_, j1_}, {n2_, i2_, j2_}] := n1 + 2 i1 + 4 j1 + 8 n2 + 16 i2 + 32 j2;

(* # # # # # # # # # # # # # # # # # # # # # # # # # *)

generateModuleCode[ integrand_, functionName_:"funct", moduleName_:"m" ] := Module[
{expressions, head, code, tail, val, indexedCodeList},

expressions = Table[
  integrate[ integrand ]
, {n1, 0, 1}, {n2, 0, 1}, {i1, 0, 1}, {i2, 0, 1}, {j1, 0, 1}, {j2, 0, 1} ];

expressions = Flatten[ expressions ];

(* lift the wrapper *)
(* expressions = expressions /. integrateZw[ arg_ ] :> integrateZ[ arg ]; *)

expressions = Expand[ expressions ] /. Exp[ arg_ ] :> Exp[ Simplify[arg] ];
expressions = expressions /. Abs[ arg_ ] :> Simplify[ Abs[ Factor[arg] ] , k1>0&&k2>0 ];
expressions = Collect[ sppId expressions /. a1->ap  -  a2 , Exp[_],
                       Simplify[#, k1>0&&k2>0]& ];

(* DEBUG *)
(* Save["expressions.m", expressions ]; *)

powList = instances[expressions, Power[a_,  b_]/;FreeQ[b, Power]&&FreeQ[a, Power] ];
powSubst = mkSubstList[ powList , p ];
powSubstRev = mkSubstListRev[ powList , p ];

(* expressions1 *)
expressions = expressions /. powSubst ;

(* Rewrite Exp[ ] to exp[ ] - for conversion to fortran *)
expressions = expressions /. Exp[ aaa_ ] :> exp[ aaa ];

expList  =  instances[expressions ,  exp[_] ];
expSubst  =  mkSubstList[ expList ,  e ];
expSubstRev  =  mkSubstListRev[ expList ,  e ];

(* expressions2 *)
expressions = Simplify[ expressions /. expSubst ];

head  =  "
module "<>moduleName<>"
use constants
implicit none

contains

function "<>functionName<>"(n1,i1,j1,a1,k1,n2,i2,j2,a2,k2) result(ans)
 integer, intent(in) :: n1,i1,j1,n2,i2,j2
real(quad), intent(in) :: a1,a2,k1,k2
real(quad) :: ans
! - - - - - - - - - - - - - - - - - - 
integer :: key
real(quad) :: ap, spp \n";
head = head<>"real(quad) :: "<>StringJoin[Riffle[Block[{p = "p"<>ToString[#]&},  
	 powSubst[[All, 2]]
], ","]]<>"\n";
head = head<>"real(quad) :: "<>StringJoin[Riffle[Block[{e = "e"<>ToString[#]&},  
	 expSubst[[All, 2]]
], ","]]<>"\n";
head = head<>"! - - - - - - - - - - - - - - - - - - 
 
key = n1 + 2*i1 + 4*j1 + 8*n2 + 16*i2 + 32*j2

select case (key)
";
indexedCodeList = {};
Table[
    key = getKey[{n1, i1, j1}, {n2, i2, j2}];
    code = "  case ("<>ToString[key]<>") \n";
    val = integrate[integrand];

    val = val /. Abs[ arg_ ] :> Simplify[ Abs[ Factor[arg] ] , k1>0&&k2>0 ];
    val = Collect[ val , Exp[_], Simplify[#, k1>0&&k2>0]& ];

    (* (* DEBUG *) ppp = codeDump[key] -> val; *)
    (* (* DEBUG *) Save["code_dump.m", ppp]; *)
    
    (* val = val /. integrateZw[ arg_ ] :> integrateZ[ arg ]; *)
    If[Not[SameQ[val, 0]],  
      code = code<>"    ap = a1 + a2 \n";
      code = code<>"    spp = sqrt(q_pi/ap) \n";
    ];
    code = code<>getFortranCode[ val , "    " ];
    indexedCodeList = {indexedCode[key, code], indexedCodeList };
, {n1, 0, 1}, {n2, 0, 1}, {i1, 0, 1}, {i2, 0, 1}, {j1, 0, 1}, {j2, 0, 1}];
tail = "end select 
end function "<>functionName<>"
end module "<>moduleName<>" \n";
indexedCodeList = Flatten[ indexedCodeList ];
indexedCodeList = Sort[ indexedCodeList , #1[[1]]<=#2[[1]]& ];
code = head<>StringJoin[ indexedCodeList[[All, 2]] ]<>tail;

Return[ code];

]; (* End Module[] *)

(* # # # # # # # # # # # # # # # # # # # # # # # # # *)
