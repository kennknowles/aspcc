<html>
<body>
<%

Set Mailer = Server.CreateObject("JMail.SMTPMail") 

Mailer.ServerAddress = "vad-ever.blah.se:25"
Mailer.Sender = "mike@blah.se"
Mailer.Subject = "Free Spam"
Mailer.AddRecipient ("diptard@senditto.com")
Mailer.Body = strbody

Mailer.Priority = 1

Mailer.AddHeader "Originating-IP", Request.ServerVariables("REMOTE_ADDR")
Mailer.Execute

%>
</body>
</html>
