$pdf_previewer = "start evince";


push @generated_exts, 'glo', 'gls', 'glg';
push @generated_exts, 'acn', 'acr', 'alg';
push @generated_exts, 'slg', 'syg', 'syi';
push @generated_exts, 'glsdefs';
push @generated_exts, 'bbl', 'fls';
$clean_ext .= ' %R.ist %R.xdy';

