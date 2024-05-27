// VAR
//     i: INTEGER;

// FOR i := 1 TO 5 DO
// BEGIN
//     DISPLAY i;
//     DISPLAY '\n';
// END.

// var
//   value: integer;
// begin
//   value := 5;
//   case value of
//     1: DISPLAY 'Valeur 1';
//     2: DISPLAY 'Valeur 2';
//     else DISPLAY 'Autre valeur';
//   end;
// end.


VAR		a,b :	INTEGER;
		c1,c2:	CHAR;
		num, denum, frac:		DOUBLE.
c1:='f';
c2:='a';
num:=1.0;
denum:=1.0;
frac:=num/denum;
a:=1;
WHILE frac>0.1 DO
BEGIN
	DISPLAY c1;
	DISPLAY '=';
	DISPLAY frac;
	DISPLAY '\n';
	DISPLAY c2;
	DISPLAY '=';
	DISPLAY a;
	DISPLAY '\n';
	denum:=denum+1.0;
	frac:=num/denum;
	a:=a+1;
	DISPLAY (a>3);
	DISPLAY '\n'
END.
