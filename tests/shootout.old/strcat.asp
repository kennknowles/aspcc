<%
	NUM = System.Argv(0)
	If NUM < 1 Then NUM = 1
	For A = 1 To NUM
		str = str & "hello" & vbCr
	Next
	Response.Write Len(str)
%>
