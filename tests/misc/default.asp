<%
	class my_array
		private m_array(10)

		public default property get item(index)
			item = m_array(index)
		end property

		public property let item(index, value)
		'	response.write "-->setting index" & index & " to " & value
			m_array(index) = value
		end property

		' TODO: add default support to 'let' and 'set'
		public sub set_val(index, value)
			m_array(index) = value
		end sub
	end class

	set x = new my_array
	x.set_val(3, "hello")
	response.write x(3) & vbNewLine
'
'	x(5) = "what is up?" & vbNewLine
'	response.write x(5)
%>
