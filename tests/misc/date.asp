<%
	response.write "vbLongDate:" & vbNewLine
	response.write FormatDateTime(date(), 1) & vbNewLine 
	response.write "vbShortDate:" & vbNewLine
	response.write FormatDateTime(date(), 2) & vbNewLine 
	response.write "vbLongTime:" & vbNewLine
	response.write FormatDateTime(date(), 3) & vbNewLine
	response.write "vbShortTime:" & vbNewLine
	response.write FormatDateTime(date(), 4) & vbNewLine
'	x = "3/6/1990"
'	response.write FormatDateTime(x,vbLongDate)


	response.write "one week from today" & vbNewLine
	response.write FormatDateTime(DateAdd("d", 7, date()), 1) & vbNewLine

	response.write "6/18/1943" & vbNewLine
	response.write FormatDateTime(DateSerial(1943, 6, 18)) & vbNewLine


	var = CDate("1/1/1900 12:45 am")
	response.write "HERE"  & vbNewLine
	response.write FormatDateTime(date(), 1) & vbNewLine
	response.write FormatDateTime(var, 0) & " vbGeneralDate <br>" & vbNewLine
	response.write FormatDateTime(var, 1) & " vbLongDate<br>" & vbNewLine
	response.write FormatDateTime(var, 2) & " vbShortDate<br>" & vbNewLine
	response.write FormatDateTime(var, 3) & " vbLongTime<br>" & vbNewLine
	response.write FormatDateTime(var, 4) & " vbShortTime<br>" & "<br>" & vbNewLine


%>
