<%
	Randomize

	response.write "Rnd with negatives:" & vbNewLine
	response.write rnd(-1) & vbNewLine
	response.write rnd(-1) & vbNewLine
	response.write rnd(-1) & vbNewLine
	response.write rnd(-1) & vbNewLine
	response.write rnd(-1) & vbNewLine

	response.write "Rnd with no params:" & vbNewLine
	response.write rnd() & vbNewLine
	response.write rnd() & vbNewLine
	response.write rnd() & vbNewLine
	response.write rnd() & vbNewLine
	
	response.write "Rnd with positive params:" & vbNewLine
	response.write rnd(1) & vbNewLine
	response.write rnd(3) & vbNewLine
	response.write rnd(3) & vbNewLine
	response.write rnd(9) & vbNewLine
	
	response.write "Rnd with zero" & vbNewLine
	response.write rnd(0) & vbNewLine
	response.write rnd(0) & vbNewLine
	response.write rnd(0) & vbNewLine
	response.write rnd(0) & vbNewLine
%>
