<%
	'Matrix Multiplication

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

	N = System.Argv(0)
	If N < 1 Then N = 1

	For I = 0 To N
		MM = mmult(size, size, M1, M2)
	Next
	Response.Write MM(0, 0) & " " & MM(2, 3) & " " & MM(3, 2) & " " & MM(4, 4)

%>
