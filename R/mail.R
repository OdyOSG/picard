


setMailServer <- function(emailAddress, password, protocol = c("gmail")) {

  mailServer <- list(
    'host' = NA_character_,
    'port' = NA_integer_,
    'username' = emailAddress,
    'password' = password
  )

  mailServer$host <- switch(protocol,
                            gmail = "smtp.gmail.com")

  mailServer$port <- switch(protocol,
                            gmail = 465L)

  return(mailServer)

}

deliverEmail <- function(email, mailServer) {


  type <- email$Type
  recipient <- switch(type,
                      'ohdsi-studies' = 'martin.lavallee@odysseusinc.com')

  emailToSend <- emayili::envelope() %>%
    emayili::from(mailServer$username) %>%
    emayili::to(recipient) %>%
    emayili::subject(email$Subject) %>%
    emayili::text(email$Body)

  smtp <- rlang::call2("server", !!!mailServer, .ns = "emayili") %>%
    eval()

  cli::cat_bullet("Sending email",
                  bullet = "tick", bullet_col = "green")
  smtp(emailToSend, verbose = FALSE)
  invisible(email)

}
