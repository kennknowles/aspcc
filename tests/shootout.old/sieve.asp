<%
'Sieve of Erathostenes

NUM = System.Argv(0)
If NUM < 1 Then NUM = 1
Dim Flags(8192)
count = 0

While NUM > 0
    NUM = NUM - 1
    count = 0
    For A = 0 To 8192
        Flags(A) = A
    Next
    For I = 2 To 8192
        If Flags(I) <> -1 Then
            For K = I+I To 8192 Step I
                Flags(K) = -1
            Next
            Count = Count + 1
        End If
    Next
Wend
Response.Write "Count: " & Count
%>
