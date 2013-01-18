<%
	z = 5

	class poop
		public x
		public y
		public num_barrels

		public default property get barrels
			barrels = num_barrels & " barrels"
		end property

		public sub change()
			x = x + 1
		end sub

		public function barf
			barf = "barf"
		end function

		public sub affect_global
			z = 21
		end sub
	end class

	set y = new poop
	y.x = 8
	response.write y.x
	response.write y.barf
	
	y.change
	call y.change()
	response.write y.x

	response.write vbNewLine

	response.write z
	y.affect_global
	response.write z

	y.num_barrels = 45
	response.write y.barrels & vbNewLine
	response.write y
%>
