
Print["\n\n"<>DateString[]<>" Fortran code generation."]
Print["Current directory is: "<>Directory[]<>"\n\n"];

Print[DateString[]<>" Loading the function definitions..."];
<< "gen_code_def.m";
Print[DateString[]<>"Done. \n"];

(* -------------------- OVERLAP ---------------------- *)
functionName = "overlap";
expr = chi[{n1,i1,j1}, a1, k1] chi[{n2,i2,j2}, a2, k2];
(* * * * * * * * * * * * * * * * *)
moduleName = "m"<>functionName;
Print[DateString[]<>" - generating code for module "<>moduleName];
fileName = functionName<>".f90";
code = generateModuleCode[ expr, functionName, moduleName ];
Export[ fileName , code , "Text" ];

(* -------------------- KINETIC ---------------------- *)
functionName = "kinetic";
expr = -1/2 chi[{n1,i1,j1}, a1, k1] D[ chi[{n2,i2,j2}, a2, k2] , {z,2}];
(* * * * * * * * * * * * * * * * *)
moduleName = "m"<>functionName;
Print[DateString[]<>" - generating code for module "<>moduleName];
fileName = functionName<>".f90";
code = generateModuleCode[ expr, functionName, moduleName ];
Export[ fileName , code , "Text" ];

(* -------------------- DIPOLE ---------------------- *)
functionName = "dipole";
expr = z chi[{n1,i1,j1}, a1, k1] chi[{n2,i2,j2}, a2, k2];
(* * * * * * * * * * * * * * * * *)
moduleName = "m"<>functionName;
Print[DateString[]<>" - generating code for module "<>moduleName];
fileName = functionName<>".f90";
code = generateModuleCode[ expr, functionName, moduleName ];
Export[ fileName , code , "Text" ];

(* -------------------- PARTIAL_Z ---------------------- *)
functionName = "partial_z";
expr = chi[{n1,i1,j1}, a1, k1] D[ chi[{n2,i2,j2}, a2, k2] , z];
(* * * * * * * * * * * * * * * * *)
moduleName = "m"<>functionName;
Print[DateString[]<>" - generating code for module "<>moduleName];
fileName = functionName<>".f90";
code = generateModuleCode[ expr, functionName, moduleName ];
Export[ fileName , code , "Text" ];

(* # # # # # # # # # # # # # # # # # # # # # # # # # *)

Print[DateString[]<>" - All done, bye."];
