<%
	class contents
		public y(10)

		public default property get item(index)
			item = y(index)
		end property

		public property let item(index, value)
			y(index) = value
		end property
	end class

	class container
		public c

		public default property get contents
			set z = c
		end property
	end class

	dim foo, baz

	set foo = new container
	set foo.c = new contents

	foo.c.item(3) = 5
	response.write foo.c.item(3)

	foo.c(3) = 5 ' DOES work
	response.write foo.contents(3)  ' doesn't work!??!?!?! AHAHAHA

	foo.contents(3) = 4  ' doesn't work???!?!?!?!
	response.write foo(3)
	
	foo(3) = 3
	response.write foo(3)  ' Doesn't work!


%>
