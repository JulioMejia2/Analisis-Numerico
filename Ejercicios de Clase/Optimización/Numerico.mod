set Tareas;
set Individuos;

var x{i in Individuos, j in Tareas} binary; #Si el individuo i hace la tarea j

param alfa{i in Individuos, j in Tareas}; #Eficiencia del individuo i realizando la tarea j

maximize Z : sum{i in Individuos, j in Tareas} x[i,j]*alfa[i,j];

subject to ASIGNACION_TAREAS {j in Tareas}: sum{i in Individuos}x[i,j]=1;
subject to INDIVIUDOS {i in Individuos}: sum{j in Tareas} x[i,j]>=1;
subject to UNICIDAD {i in Individuos, j in Tareas}: x[i,j] <=1;



solve;

data;

set Individuos:= 'A','B','C';
set Tareas:= 'A','B','C','D';

param alfa: 'A' 	 'B' 	 'C' 	 'D' :=
'A' 	     0  	  7 	  8 	 11
'B' 	     6  	  0 	  9 	  7
'C' 	     9  	  8 	  0 	  5;

end;
