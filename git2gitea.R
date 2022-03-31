# melhor versão
#...............................................................................
# criar issue + editar o que for necessário + adicionar comentários 

# criando novo usuário
# body = as.list(data.frame(email='luis.galhardo@usp.br', full_name='Luis Galhardo',
#                           login_name='galhardo', "must_change_password"=TRUE,
#                           password='luis',send_notify=TRUE, 
#                           username="galhardo"))
# 
# POST("https://seiscode.iag.usp.br/api/v1/admin/users", 
#      add_headers(Authorization = authorization, sudo=author), 
#      content_type_json(), encode = "json", body=body)
# DELETE("https://seiscode.iag.usp.br/api/v1/admin/users/teste",
#        content_type_json(), encode = "json",
#        add_headers(Authorization = authorization, sudo=author))

rm(list=ls())

library(httr)
library(jsonlite)
library(gitear)  # much simpler only body and title



#---- functions ----
request_to_edit <- function(pagina, iid){
  pagina <- pagina[pagina$iid == iid, ]
  
  userDict <- data.frame(git = c('daniel','bruno','NA','NA','emilia.brasilio', 'NA', 'jackson','NA','NA', 
                                 'm.tchelo','NA','galhardo'), 
                         gitea = c('Daniel','bruno', 'caiao','caio','emilia','hernan','jackson','katerine',
                                   'mariana','mbianchi','seistech','galhardo'))
  assignee <- userDict$gitea[userDict$git %in% unlist(pagina$assignee$username)]
  if(length(assignee) == 0 ) assignee = ""
  # ---- body ----
  body <- unlist(pagina$description)
  
  # ---- state ----
  state <- unlist( pagina$state )
  
  # ---- milestone ----
  milesDict <- data.frame(git = c('Campo','STORES','QC','dataless','SDS'),
                          gitea = c('Campo','Store', 'QC','Dataless','SDS'))
  
  title <- milesDict$gitea[ milesDict$git %in% unlist(pagina$milestone$title)] 
  
  milestone <- g_miles$id[ g_miles$title == title ] # id do author no gitea
  if( length(milestone) != 1 ) {
    milestone <- 0
  }
  
  #---- due_date ----
  #due_date <- unlist( strftime()pagina$created_at)
  
  #---- title ----
  title <- unlist(pagina$title)
  
  ref <- ""
  
  request_body <- as.list(
    data.frame(
      assignee=assignee, body=body, #due_date=due_date, 
      milestone=milestone,
      state=state, title=title
    ))
  
  return(request_body)
}

request_to_notes <- function(pagina, n){
  
  
  created_at = unlist(pagina$created_at)[n]
  body = paste(created_at, unlist(pagina$body)[n])
  notes <- as.list(
    data.frame(
      body = body, created_at = created_at
    ))
  return(notes)
}

person_to_inform <- function(git_user){
  userDict <- data.frame(git = c('daniel','bruno','NA','NA','emilia.brasilio', 'NA', 'jackson','NA','NA', 
                                 'm.tchelo','NA','galhardo','jroberto'), 
                         gitea = c('Daniel','bruno', 'caiao','caio','emilia','hernan','jackson','katerine',
                                   'mariana','mbianchi','seistech','galhardo','jroberto'))
  gitea_user <- userDict$gitea[userDict$git == git_user]
  return(gitea_user)
}


#---- credentials ----
# gitlab 
git_url <-   'http://oldseiscode.sismo.iag.usp.br/gitlab/api/v3/projects/160/issues'
git_token <- 'rK6tvUNWgow7dX5u4Y1n'
issue <- paste0(git_url,'?private_token=', git_token)
#comments <- 'http://oldseiscode.sismo.iag.usp.br/gitlab/api/v3/projects/160/issues/118/notes?private_token=rK6tvUNWgow7dX5u4Y1n'


# gitea
giteaToken <- 'd69519da6d88b1d986ad8f594cfdb54f3847587c'
url_seiscode <- 'https://seiscode.iag.usp.br/api/v1/repos/CSUSP/notepad4stations/issues'
authorization <- paste("token", giteaToken)

# comentários dos tickets
comm  <- 'https://seiscode.iag.usp.br/api/v1/repos/CSUSP/notepad4stations/issues/comments'
label <- 'https://seiscode.iag.usp.br/api/v1/repos/CSUSP/notepad4stations/labels'

tgCom <- fromJSON(toJSON(content(GET(comm, add_headers(Authorization = authorization)))))
labels <-fromJSON(toJSON(content(GET(label, add_headers(Authorization = authorization)))))

# features
g_users <- get_list_users(base_url = 'https://seiscode.iag.usp.br', 
                          api_key = giteaToken)

g_miles <- get_milestones(base_url = 'https://seiscode.iag.usp.br', 
                          api_key = giteaToken, owner = 'CSUSP',
                          repo = 'notepad4stations')


#.................................................................
#---- lendo do git ----
#.................................................................
options(stringsAsFactors = FALSE)

for(i in 49:1){ # pages

#i <- 1
  t1 <- fromJSON(toJSON(content(GET(issue, query=list(page=i),flatten=TRUE ))))
  t2 <- which(t1$state != 'closed')
  #t2 <- 1:nrow(t1)
  
  for(a in sort(t2,decreasing = TRUE)) {
    ticket = unlist(t1$iid[a])
    cat('\n *** Migrando ticket ', ticket ,' ***\n')
    request_body <- request_to_edit(t1, unlist(t1$iid[a]))
    author = person_to_inform(t1$author$username[a])
    POST(url_seiscode, 
         add_headers(Authorization = authorization, sudo=author), 
         content_type_json(), encode = "json", body = request_body)
    
    # comentários
    note = t1$id[t1$iid ==  unlist(t1$iid[a])]
    comm <- paste0(git_url, '/',note, '/notes?private_token=', git_token)
    c1 <- fromJSON(toJSON(content(GET(comm))))
    
    if( length(c1) != 0){
      # inserir comentários
      tg <-fromJSON(toJSON(content(GET(url_seiscode, add_headers(Authorization = authorization)))))
      pp <- which(unlist(tg$title) == t1[a,]$title)
      
      
      for (n in nrow(c1):1){
        gt_comm <- 
          paste0('https://seiscode.iag.usp.br/api/v1/repos/CSUSP/notepad4stations/issues/',
                 #unlist(t1$iid[a]),
                 tg$number[pp[1]],'/comments')
        request_notes <- request_to_notes(c1, n)
        
        if(!is.null(unlist(c1$attachment[n]))){
          cat(t1$iid[a], c1[n])
        }
        
        com_author = person_to_inform(c1$author$username[n])
        uia <- POST(gt_comm, add_headers(Authorization = authorization, sudo=com_author), 
                    content_type_json(), encode = "json", body = request_notes)
        cat('comentário ', n, '\n')
      } # inserir comentários
    } # verifica se tem comentários
    
      
      # se for ticket fechado, manter fechado
      if( t1$state[a] == "closed"){
        cat("\n fechando ticket ",t1$iid[a], '\n')
        PATCH(url_seiscode, add_headers(Authorization = authorization, sudo=com_author), 
                content_type_json(), encode = "json", body = as.list(data.frame(state='closed')))
      }  

    } # for para as linhas da página
  
  
 }  # pages




#.........................................................................  
# para editar comentários: PATCH /repos/{owner}/{repo}/issues/comments/{id}

