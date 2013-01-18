<%
	class my_array
		private m_array(10)

		' this is a weird problem, because it should assign it
		' to the return value rather than doing a 'property let'
		public default property get item(index)
			item = m_array(index)
		end property

		public default property let item(index, value)
		'	response.write "-->setting index" & index & " to " & value
			m_array(index) = value
		end property

		' TODO: add default support to 'let' and 'set'
		public sub set_val(index, value)
			m_array(index) = value
		end sub
	end class

	set x = new my_array
	x(5) = "what is up?" & vbNewLine
	response.write x(5) & vbNewLine
'
'	response.write x(5)
%>
