<%
	tot = 0
	Blob = WScript.StdIn.ReadAll
	Nums = Split(Blob, Chr(10))
	for each num in nums
	    tot = tot + CInt(num)
	Next
	WScript.Echo(tot)
%>
