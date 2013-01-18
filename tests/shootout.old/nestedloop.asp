<%
	'Nested Loops

	n = System.Argv(0)
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

	response.write x
%>
