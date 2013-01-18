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
