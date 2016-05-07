## Part I- Scrape election info from politico.com
# Team 5- Stat323 Spring 2016

suppressMessages(library(rvest))
suppressMessages(library(magrittr))
suppressMessages(library(dplyr))
suppressMessages(library(stringr)) 
suppressMessages(library(maps))

url = "http://www.politico.com/2016-election/results/map/president"

page = read_html(url)

#Scrape names of all the states on the website.
states = page %>% html_nodes(".timeline-header a") %>% html_text(trim = T)

#Scrape data, including names of states, candidates' name, percentage, delegates.
data = page %>% html_nodes("h5 , h6 , .number , .name-combo , .timeline-header a,.delegates-cell") %>% html_text(trim = T)

#Because our data is a long vector, we use get_position function to find out the index
get_position = function(x){
  result = which(data == x)
  return (result)
}
index = sapply(states, get_position)

Dele.Clinton = c()
Dele.Trump = c()
Dele.sanders = c()
Dele.Tcruz = c()
Dele.Jkasich = c()

H.Clinton = c()
D.Trump = c()
B.sanders = c()
Tcruz = c()
Jkasich = c()

Win_Demo = c()
Win_Repu = c()

for(i in seq_along(index)){
  if(i<length(index)){
    sub_data = data[index[i]:(index[i+1]-1)]
  }else{
    sub_data = data[index[i]:length(data)]
  }
  
  boolean_clinton = T
  boolean_trump = T
  boolean_sanders = T
  boolean_cruz = T
  boolean_kasich = T
  
  boolean_demo = T
  boolean_repu = T
  
  for(i in seq_along(sub_data)){
    if(sub_data[i] == "Democratic"){
      boolean_demo = F
      Win_Demo = append(Win_Demo, sub_data[i+1])
    }
    
    if(sub_data[i] == "Republican"){
      boolean_repu = F
      Win_Repu = append(Win_Repu, sub_data[i+1])
    }
    
    if(grepl("H. Clinton",sub_data[i])){
      boolean_clinton = F
      
      percentage = as.numeric(sub("%","",sub_data[i+1]))/100
      H.Clinton = append(H.Clinton, percentage)
      
      if(sub_data[i+2] != ""){
        delegate = as.numeric(sub_data[i+2])
        Dele.Clinton = append(Dele.Clinton, delegate)
      }else{
        Dele.Clinton = append(Dele.Clinton, 0)
      }
    }
    
    if(grepl("D. Trump",sub_data[i])){
      boolean_trump = F
      
      percentage = as.numeric(sub("%","",sub_data[i+1]))/100
      D.Trump = append(D.Trump, percentage) 
      
      if(sub_data[i+2] != ""){
        delegate = as.numeric(sub_data[i+2])
        Dele.Trump = append(Dele.Trump, delegate)
      }else{
        Dele.Trump = append(Dele.Trump, 0)
      }
    }
    
    if(grepl("B. Sanders",sub_data[i])){
      boolean_sanders = F
      
      percentage = as.numeric(sub("%","",sub_data[i+1]))/100
      B.sanders = append(B.sanders, percentage) 
      
      if(sub_data[i+2] != ""){
        delegate = as.numeric(sub_data[i+2])
        Dele.sanders = append(Dele.sanders, delegate)
      }else{
        Dele.sanders = append(Dele.sanders, 0)
      }
    }
    
    if(grepl("T. Cruz",sub_data[i])){
      boolean_cruz = F
      
      percentage = as.numeric(sub("%","",sub_data[i+1]))/100
      Tcruz = append(Tcruz, percentage) 
      
      if(sub_data[i+2] != ""){
        delegate = as.numeric(sub_data[i+2])
        Dele.Tcruz = append(Dele.Tcruz, delegate)
      }else{
        Dele.Tcruz = append(Dele.Tcruz, 0)
      }
    }
    
    if(grepl("J. Kasich",sub_data[i])){
      boolean_kasich = F
      
      percentage = as.numeric(sub("%","",sub_data[i+1]))/100
      Jkasich = append(Jkasich, percentage) 
      
      if(sub_data[i+2] != ""){
        delegate = as.numeric(sub_data[i+2])
        Dele.Jkasich = append(Dele.Jkasich, delegate)
      }else{
        Dele.Jkasich = append(Dele.Jkasich, 0)
      }
    }
  }
  
  if(boolean_clinton){
    H.Clinton = append(H.Clinton, 0)
    Dele.Clinton = append(Dele.Clinton, 0)
  }
  if(boolean_trump){
    D.Trump = append(D.Trump, 0)
    Dele.Trump = append(Dele.Trump, 0)
  }
  if(boolean_sanders){
    B.sanders = append(B.sanders, 0)
    Dele.sanders = append(Dele.sanders, 0)
  }
  if(boolean_cruz){
    Tcruz = append(Tcruz, 0)
    Dele.Tcruz = append(Dele.Tcruz, 0)
  }
  if(boolean_kasich){
    Jkasich = append(Jkasich, 0)
    Dele.Jkasich = append(Dele.Jkasich, 0)
  }
  if(boolean_repu){
    Win_Repu = append(Win_Repu, "Result Coming soon")
  }
  if(boolean_demo){
    Win_Demo = append(Win_Demo, "Result Coming soon")
  }
}

df0 = data.frame(
  state = states,
  
  clinton = H.Clinton,
  Dele_Clinton = Dele.Clinton,
  
  trump = D.Trump,
  Dele_Trump = Dele.Trump,
  
  sanders = B.sanders,
  Dele_Sanders = Dele.sanders,
  
  cruz = Tcruz,
  Dele_Cruz = Dele.Tcruz,
  
  kasich = Jkasich,
  Dele_Kasich = Dele.Jkasich,
  
  D_win = Win_Demo,
  R_win = Win_Repu,
  
  stringsAsFactors = F
)

states = map("state", plot=FALSE, fill=TRUE)
state.name = as.data.frame(states$names, stringsAsFactors = FALSE)
colnames(state.name) = "state"

df0[,1] = tolower(df0[,1])

df = left_join(state.name, df0, by="state")

df$state2 = str_split(df$state, ":", 2)
for(i in seq_along(df$state2)){
  df$state2[i] = df$state2[[i]][1]
}

for(i in seq_along(df$state2)){
  for(j in seq_along(df0$state)){
    if(df$state2[i] == df0$state[j]){
      df[i,2:13] = df0[j,2:13]
    }
  }
}

save(df0, file = "df0.Rdata")
save(df, file = "df.Rdata")
