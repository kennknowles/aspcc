<%
	'Object Instantiation

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

Set oToggle = New Toggle
oToggle.Bool = 1
For A = 1 To 5
    oToggle.Activate
    If oToggle.Value Then    
        Response.Write "true"
    Else
        Response.Write "false"
    End If
Next
For A = 1 To NUM
    Set oToggle = New Toggle
    oToggle.Bool = 1
Next

Response.Write vbNewLine

Set onToggle = New NthToggle
onToggle.Bool = 1
onToggle.CountMax = 3
For A = 1 To 8
    onToggle.Activate
    If onToggle.Value Then    
        Response.Write "true"
    Else
        Response.Write "false"
    End If
Next
For A = 1 To NUM
    Set onToggle = New NthToggle
    onToggle.Bool = 1
    onToggle.CountMax = 3
Next
%>
