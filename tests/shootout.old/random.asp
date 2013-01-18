Ackermann's Function

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

NUM = WScript.Arguments(0)
If NUM < 1 Then NUM = 1
nack = Ack(3, NUM)
WScript.Echo "Ack(3," & NUM & "): " & nack

Array Access


n = WScript.Arguments(0)
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

WScript.Echo Y(0) & " " & Y(last)

Fibonacci Numbers

Function Fib(N)
    If N < 2 Then
        Fib = 1
    Else
        Fib = Fib(N-2) + Fib(N-1)
    End If
End Function

NUM = WScript.Arguments(0)
If NUM < 1 Then NUM = 1
nfib = Fib(NUM)
WScript.Echo nfib

Hash (Associative Array) Access

n = WScript.Arguments(0)
If n < 1 Then n = 1

Set X = CreateObject("Scripting.Dictionary")
c = 0
For i = 1 To N
    X.Add Hex(i), i
Next

For i = n To 1 step -1
    If X.Exists(CStr(i)) Then c = c + 1
Next
WScript.Echo c

Hashes, Part II

n = WScript.Arguments(0)
If n < 1 Then n = 1

Set hash1 = CreateObject("Scripting.Dictionary")
For i = 0 To 9999
    hash1.Add "foo_" & i, i
Next

Set hash2 = CreateObject("Scripting.Dictionary")
For i = 1 To N
    For Each k In hash1.Keys
        If Not hash2.Exists(k) Then
            hash2.Add k, 0
        End If
        hash2.Item(k) = hash2.Item(k) + hash1.Item(k)
    Next
Next

WScript.Echo hash1.Item("foo_1") & " " & hash1.Item("foo_9999") & " " & hash2.Item("foo_1") & " " & hash2.Item("foo_9999")

Heapsort

Const IM = 139968
Const IA =   3877
Const IC =  29573

LAST = 42

Function gen_random(n)
    LAST = (LAST * IA + IC) Mod IM
    gen_random = n * LAST / IM
End Function


Sub heapsort(n, ra)
    rra = 0
    i = 0
    j = 0
    l = CLng((n / 2) + 1)
    ir = n
    
    While 1
        If l > 1 Then
            l = l - 1
            rra = ra(l)
        Else
            rra = ra(ir)
            ra(ir) = ra(1)
            ir = ir - 1
            If ir = 1 Then
                ra(1) = rra
                Exit Sub
            End If
        End If
        
        i = l
        j = l * 2

        While  CLng(j) <= CLng(ir)
            If CLng(j) < CLng(ir) Then
                If ra(j) < ra(j+1) Then j = j + 1
            End If
            
            If rra < ra(j) Then
                ra(i) = ra(j)
                i = j
                j = j + i
            Else
                j = ir + 1
            End If
        Wend
        ra(i) = rra
    Wend    

End Sub

n = WScript.Arguments(0)
If n < 1 Then n = 1

Redim ary(N+1)

For i = 1 To N
    ary(i) = gen_random(1)
Next

heapsort N, ary

WScript.Echo FormatNumber(ary(N), 10)

Hello World

WScript.Echo( "hello world" )

List Operations

Const SIZE = 10000

ITER = WScript.Arguments(0)
If ITER < 1 Then ITER = 1

result = 0
While ITER
    result = test_lists
    ITER = ITER - 1
Wend
WSCript.Echo result

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
        'WScript.Echo "I=" & i & " Li1=" & Li1(i) & " Li2=" & Li2(i)
        If Li1(i) <> Li2(i) Then
            test_lists = 0
            Exit Function
        End If
    Next
    test_lists = UBound(Li1)   
End Function

Matrix Multiplication

Const size = 30

Function mkmatrix(rows, cols)
    ReDim mx(size, size)
    rows = rows - 1
    cols = cols - 1
    count = 1
    For R = 0 To rows
        For C = 0 To cols
            mx(R, C) = count
            count = count + 1
        Next
    Next
    mkmatrix = mx
End Function

Function mmult(rows, cols, m1, m2)
    ReDim m3(size, size)
    rows = rows - 1
    cols = cols - 1
    
    For i = 0 To rows
        For j = 0 To cols
            val = 0
            For k = 0 To cols
                val = val + m1(i, k) * m2(k, j)
            Next
            m3(i, j) = val
        Next
    Next
    mmult = m3
End Function

M1 = mkmatrix(size, size)
M2 = mkmatrix(size, size)

N = WScript.Arguments(0)
If N < 1 Then N = 1

For I = 0 To N
    MM = mmult(size, size, M1, M2)
Next
WScript.Echo MM(0, 0) & " " & MM(2, 3) & " " & MM(3, 2) & " " & MM(4, 4)

Method Calls

Class Toggle

    Public Bool

    Public Property Get value() 
        value = Bool
    End Property 

    Public Property Let value(v) 
        Bool = v
    End Property 

    Public Sub activate()
        If Bool Then
            Bool = False
        Else
            Bool = True
        End If
    End Sub

End Class 

Class NthToggle

    Public Bool
    Private Counter
    Public CountMax
    
    Public Property Get value() 
        value = Bool
    End Property 

    Public Property Let value(v) 
        Bool = v
    End Property 
    
    Public Sub activate()
        Counter = Counter + 1
        If Counter >= CountMax Then
            If Bool Then
                Bool = False
            Else
                Bool = True
            End If
            Counter = 0
        End If
    End Sub

End Class

NUM = WScript.Arguments(0)
If NUM < 1 Then NUM = 1
val = 1
Set oToggle = New Toggle
oToggle.Bool = val
For I = 1 To NUM
    oToggle.Activate
    val = oToggle.Value
Next
If val Then
    WScript.Echo "true"
Else
    WScript.Echo "false"
End If

val = 1
Set onToggle = New NthToggle
onToggle.Bool = val
onToggle.CountMax = 3
For I = 1 To NUM
    onToggle.Activate
    val = onToggle.Value
Next
If val Then
    WScript.Echo "true"
Else
    WScript.Echo "false"
End If

Nested Loops

n = WScript.Arguments(0)
If n < 1 Then n = 1
x = 0
a = n
While a
    b = n
    While b
        c = n
        While c
            d = n
            While d
                e = n
                While e
                    f = n
                    while f
                        x = x + 1
                        f = f - 1
                    Wend
                    e = e - 1
                Wend
                d = d - 1
            Wend
            c = c - 1
        Wend
        b = b - 1
    Wend
    a = a -1
Wend

WScript.Echo x

Object Instantiation

Class Toggle

    Public Bool

    Public Property Get value() 
        value = Bool
    End Property 

    Public Property Let value(v) 
        Bool = v
    End Property 

    Public Sub activate()
        If Bool Then
            Bool = False
        Else
            Bool = True
        End If
    End Sub

End Class 

Class NthToggle

    Public Bool
    Private Counter
    Public CountMax
    
    Public Property Get value() 
        value = Bool
    End Property 

    Public Property Let value(v) 
        Bool = v
    End Property 
    
    Public Sub activate()
        Counter = Counter + 1
        If Counter >= CountMax Then
            If Bool Then
                Bool = False
            Else
                Bool = True
            End If
            Counter = 0
        End If
    End Sub

End Class

NUM = WScript.Arguments(0)
If NUM < 1 Then NUM = 1

Set oToggle = New Toggle
oToggle.Bool = 1
For A = 1 To 5
    oToggle.Activate
    If oToggle.Value Then    
        WScript.Echo "true"
    Else
        WScript.Echo "false"
    End If
Next
For A = 1 To NUM
    Set oToggle = New Toggle
    oToggle.Bool = 1
Next

WScript.Echo

Set onToggle = New NthToggle
onToggle.Bool = 1
onToggle.CountMax = 3
For A = 1 To 8
    onToggle.Activate
    If onToggle.Value Then    
        WScript.Echo "true"
    Else
        WScript.Echo "false"
    End If
Next
For A = 1 To NUM
    Set onToggle = New NthToggle
    onToggle.Bool = 1
    onToggle.CountMax = 3
Next

Random Number Generator

Const IM = 139968
Const IA =   3877
Const IC =  29573

LAST = 42

Function gen_random(n)
    LAST = (LAST * IA + IC) Mod IM
    gen_random = n * LAST / IM
End Function

result = 0
n = WScript.Arguments(0)
If n < 1 Then n = 1
For i = 1 To N
    result = gen_random(100)
Next

WScript.Echo FormatNumber(result, 9)

Regular Expression Matching

Set regEx = new RegExp
regEx.Pattern = "^[^\d\(]*(\(\d\d\d\)|\d\d\d) (\d\d\d)[- ](\d\d\d\d)(\D|$)"
regEx.Global = True

phonesBlob = WScript.Stdin.ReadAll()
phones = Split(phonesBlob, Chr(10))

N = WScript.Arguments(0)
If N < 1 Then N = 1

While N > 0
    For Each line in phones
        Set Matches = regEx.Execute(line)
        If Matches.Count > 0 Then
            ' WSCript.Echo "[" & Matches.Count & "]" & line
            tel1 = Matches(0).Submatches(0)
            If Left(tel1, 1) = "(" Then
                tel1 = Mid(tel1, 2, Len(tel1)-2)
            End If
            tel2 = Matches(0).Submatches(1)
            tel3 = Matches(0).Submatches(2)
            num = "(" & tel1 & ") " & tel2 & "-" & tel3
            If N = 1 Then
                Count = Count + 1
                WScript.Echo Count & ": " & num
            End If
        Else
            ' WScript.Echo "nomatch: " & line
        End If
    Next
    N = N - 1
Wend

Reverse a File

FileBlob = WScript.StdIn.ReadAll
Lines = Split(FileBlob, Chr(10))
For A = UBound(Lines) To LBound(Lines) Step -1
    If Len(Lines(A)) > 0 Then WScript.Echo Lines(A)
Next

Sieve of Erathostenes

NUM = WScript.Arguments(0)
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
WScript.Echo "Count: " & Count

Statistical Moments

<job>

<script language=JScript runat=server>

    function SortNumeric(a, b) {
        return ((+a > +b) ? 1 : ((+a < +b) ? -1 : 0));
    }

    function SortVBArray(arrVBArray) {
        return arrVBArray.toArray().sort(SortNumeric).join('x');
    }
</script>

<script language=VBScript>

Function SortArray(arrInput)
    SortArray = Split(SortVBArray(arrInput), "x")
End Function

Sum = 0
N = 0
NumBlob = WScript.StdIn.ReadAll
Num = Split(NumBlob, vbCrLf)
N = UBound(Num)

For A = 0 To N - 1
    Sum = Sum + Num(A)
Next

mean = sum / N
average_deviation = 0
standard_deviation = 0
variance = 0
skew = 0
kurtosis = 0
For A = 0 To N - 1
    deviation = Num(A) - mean
    average_deviation = average_deviation + Abs(deviation)
    variance = variance + deviation^2
    skew = skew + deviation^3
    kurtosis = kurtosis + deviation^4
Next
average_deviation = average_deviation / N
variance = variance / (N-1)
standard_deviation = Sqr(variance)
If variance Then
    skew = skew / (N * variance * standard_deviation)
    kurtosis = kurtosis / (n * variance * variance ) - 3.0
End If

SortNum = SortArray(Num)

middle = N/2 + 1

If (N Mod 2) Then
    median = CInt(SortNum(middle))
Else
    median = (CInt(SortNum(middle)) + CInt(SortNum(middle-1))) / 2
End If

WScript.Echo "n:                  " & N
WScript.Echo "median:             " & FormatNumber(median, 6, -1, 0, 0)
WScript.Echo "mean:               " & FormatNumber(mean, 6, -1, 0, 0)
WScript.Echo "average_deviation:  " & FormatNumber(average_deviation, 6, -1, 0, 0)
WScript.Echo "standard_deviation: " & FormatNumber(standard_deviation, 6, -1, 0, 0)
WScript.Echo "variance:           " & FormatNumber(variance, 6, -1, 0, 0)
WScript.Echo "skew:               " & FormatNumber(skew, 6, -1, 0, 0)
WScript.Echo "kurtosis:           " & FormatNumber(kurtosis, 6, -1, 0, 0)
</script>
</job>

String Concatenation

NUM = WScript.Arguments(0)
If NUM < 1 Then NUM = 1
For A = 1 To NUM
    str = str & "hello" & vbCr
Next
WScript.Echo Len(str)

Sum a Column of Integers

On Error Resume Next
tot = 0
Blob = WScript.StdIn.ReadAll
Nums = Split(Blob, Chr(10))
for each num in nums
    tot = tot + CInt(num)
Next
WScript.Echo(tot)

Word Frequency Count

<job>

<script language=JScript runat=server>

    function Descending(a, b) {
        return ((b > a) ? 1 : ((b < a) ? -1 : 0));
    }

    function SortVBArray(arrVBArray) {
        return arrVBArray.toArray().sort(Descending).join('@');
    }
</script>

<script language=VBScript>

Function SortArray(arrInput)
    SortArray = Split(SortVBArray(arrInput), "@")
End Function

Set Count = CreateObject("Scripting.Dictionary")

Blob = WScript.StdIn.ReadAll

Lines = Split(Blob, vbCrLf)

For Each L in Lines
    Line = Trim(LCase(L))
    For B = 1 To Len(Line)
        C = Asc(Mid(Line, B, 1))
        If C <> Asc(" ") And (C < Asc("a") Or C > Asc("z")) Then
            'WSCript.Echo(Line)
            'WScript.Echo(String(B-1, " ") & "^")
            Line = Left(Line, B-1) & " " & Mid(Line, B+1)
            'WSCript.Echo(Line)
            'WScript.Echo(String(B-1, " ") & "^")
        End If
    Next

    Words = Split(Line, " ")
    For Each Word in Words
        If Word <> " " And Word <> "" Then 
            If Count.Exists(Word) Then
                Count.Item(Word) = Count.Item(Word) + 1
            Else
                Count.Item(Word) = 1
            End If
        End If
    Next    
Next

K = Count.Keys
Redim Lines(Count.Count-1)

For A = 0 To Count.Count-1
    N = CStr(Count.Item(K(A)))
    If Len(N) < 7 Then N = String(7-Len(N), " ") & N
    Lines(A) = N & Chr(9) & K(A)
Next

SortedLines = SortArray(Lines)
For A = LBound(SortedLines) To UBound(SortedLines)
    WScript.Echo(SortedLines(A))
Next
    
</script>
</job>
