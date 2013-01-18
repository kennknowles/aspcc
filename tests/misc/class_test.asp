<%

Dim Y

Class mike
  Private X
  Public K

  Public Property Let MyProp(pA)
    a1= pA
  End Property


  Public Sub MySub()

    K=K+1
  End Sub

End Class

mymike=new mike

mymike.MySub()
mymike.Y=500

Response.Write mymike.Y
Response.Write mymike.MySub()

%>
