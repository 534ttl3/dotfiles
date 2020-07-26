$pdf_previewer = "start zathura";


push @generated_exts, 'glo', 'gls', 'glg';
push @generated_exts, 'acn', 'acr', 'alg';
push @generated_exts, 'slg', 'syg', 'syi';
push @generated_exts, 'glsdefs';
push @generated_exts, 'bbl', 'fls';
$clean_ext .= ' %R.ist %R.xdy';


sub asy {return system("asy -o asypdf/ '$_'");}
add_cus_dep("asy","eps",0,"asy");
add_cus_dep("asy","pdf",0,"asy");
add_cus_dep("asy","tex",0,"asy");
