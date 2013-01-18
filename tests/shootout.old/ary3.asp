<%
	' Array Access

	n = System.Argv(0)
	Redim X(n), Y(n)

	If n < 1 Then n = 1
	last = n - 1
	For i = 0 To last
		X(i) = i + 1
	Next
	For K = 0 To 999
		For I = last To 0 Step -1
			Y(i) = Y(i) + X(i)
		Next
	Next

	Response.Write Y(0) & " " & Y(last)
%>
