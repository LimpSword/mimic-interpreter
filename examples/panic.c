a := 1;
b := 2;
if true then {
    b := 8;
    print b;
    panic "hey";
    print "this will not be printed";
} else { 
    skip;
}

