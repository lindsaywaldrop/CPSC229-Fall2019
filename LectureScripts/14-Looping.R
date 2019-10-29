u=1
n=1
while(u<10){
  n=n+1
  message(u)
  message(n)
}

repeat{
  message("This is the song that never ends")
  message("Yes, it goes on and on my friend,")
  message("Some people started singing it, not knowing what it was,")
  message("And now they are still singing it just because...")
}

repeat{
  message("Happy Groundhog's Day")
  action<-sample(
    c(
      "Learn French",
      "Make an ice statue",
      "Rob a bank",
      "Win heart of Andie McDowell"
    ),
    1
  )
  message("action = ",action)
  if(action == "Win heart of Andie McDowell"){
    message("Yay!")
    break
    }
}

for(var in seq(1,5,by=1)){
  print(var)
}

data(beavers)
for(i in 1:length(beaver1$time)){
  message("At time ",beaver1$time[i]," the beaver's temperature was ",beaver1$temp[i])
}
  