<%
	' Ackermann's Function

	Function Ack(M, N)
		If M = 0 Then
			Ack = N + 1
		Else
	        If N = 0 Then
	            Ack = Ack(M-1, 1)
	        Else
            Ack = Ack(M-1, Ack(M, N-1))
			End If
		End If
	End Function

	'NUM = WScript.Arguments(0)
	NUM = System.Argv(0)
	If NUM < 1 Then NUM = 1
	nack = Ack(3, NUM)
	'WScript.Echo "Ack(3," & NUM & "): " & nack
	Response.Write "Ack(3," & NUM & "): " & nack
%>
