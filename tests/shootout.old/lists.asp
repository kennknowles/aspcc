<%
'List Operations

Const SIZE = 10000

ITER = System.Argv(0)
If ITER < 1 Then ITER = 1

result = 0
While ITER
    result = test_lists
    ITER = ITER - 1
Wend
Response.Write result & vbNewLine

Function test_lists()
    ' create a list of integers (Li1) from 1 to SIZE
    Redim Li1(SIZE), Li2(SIZE), Li3(SIZE), Li4(SIZE)
    For A = LBound(Li1) To UBound(Li1)
        Li1(A) = A
    Next
    ' copy the list to Li2 (not by individual items)
    For A = LBound(Li1) To UBound(Li1)
        Li2(A) = Li1(A)
    Next
    ' remove each individual item from left side of Li2 and
    ' append to right side of Li3 (preserving order)
    For A = LBound(Li2) To UBound(Li2)
        Li3(A) = Li2(SIZE-A)
    Next
    
    ' Li2 must now be empty
    ' remove each individual item from right side of Li3 and
    ' append to right side of Li2 (reversing list)
    
    For A = LBound(Li2) To UBound(Li2)
        Li2(A) = Li3(A)
    Next
        
    ' Li3 must now be empty
    ' reverse Li1 in place
    For A = LBound(Li1) To UBound(Li1)
        Li4(A) = Li1(SIZE-A)
    Next
    For A = LBound(Li4) To UBound(Li4)
        Li1(A) = Li4(A)
    Next
    ' check that first item is now SIZE
    If Li1(0) <> SIZE Then
        test_lists = -1
        Exit Function
    End If
    
    ' compare Li1 and Li2 for equality
    For i = LBound(Li1) To UBound(Li1)
        'Response.Write "I=" & i & " Li1=" & Li1(i) & " Li2=" & Li2(i)
        If Li1(i) <> Li2(i) Then
            test_lists = 0
            Exit Function
        End If
    Next
    test_lists = UBound(Li1)   
End Function
%>
