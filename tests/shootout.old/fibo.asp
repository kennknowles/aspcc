<%
'Fibonacci Numbers

Function Fib(N)
    If N < 2 Then
        Fib = 1
    Else
        Fib = Fib(N-2) + Fib(N-1)
    End If
End Function

NUM = System.Argv(0)
If NUM < 1 Then NUM = 1
nfib = Fib(NUM)
Response.Write nfib
%>
