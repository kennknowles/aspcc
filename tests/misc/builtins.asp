<%
	call response.write( "hello" )
	response.write "hi" & vbnewline

	response.write "abs -3 is 3 = " & abs(-3) & vbnewline
'	x = array(4, 5, 6, 8, "hello", -3.5, false)
'	x = asc("a")
'	x = atn(0.5)
'	x = cbool(true)
'	x = cbyte("45")
'	x = ccur("87.004")
'	x = cdate("4/7/1925")
'	x = cdbl(68)
'	x = chr(58)
'	x = cint("-48")
'	x = clng("9458")
	response.write "cos 4.689 = -0.0233868479745465 " & cos(4.689) & vbnewline
	response.write "sin 4.689 = -0.999726490267121  " & sin(4.689) & vbnewline
	response.write "tan 4.689 = 42.747381  " & tan(4.689) & vbnewline
'	x = csng(4.583)
'	x = date()
'	x = dateadd("y", 31, "4/6/1990")
'	x = datediff("m", "1/1/2002", "2/3/2003")
'	x = datepart("yyyy", "7/13/1945")
'	x = dateserial(1927, 8, 24)
'	x = datevalue("7/9/1842")
'	x = day("9/3/01")

	response.write IsNumeric(4)	& vbNewLine
	response.write IsNumeric("hey buddy") & vbNewLine
%>
