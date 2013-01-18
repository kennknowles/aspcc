<%
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

	NUM = System.Argv(0)
	If NUM < 1 Then NUM = 1
	val = 1
	Set oToggle = New Toggle
	oToggle.Bool = val
	For I = 1 To NUM
		oToggle.Activate
		val = oToggle.Value
	Next
	If val Then
		Response.Write "true"
	Else
		Response.Write "false"
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
		Response.Write "true"
	Else
		Response.Write "false"
	End If
%>
