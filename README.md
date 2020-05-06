# ADS Project 5: Model of Virus Spreading Process

![GIF](output/Bad1.gif)

**Term: Spring 2020**
+ Team #1
+ Project title: Virus spreading model
+ Team members
	+ Shuxin Chen
	+ Junyan Guo
	+ Vikki Sui
	+ Jinxu Xiang
	+ Ziqin Zhao
+ Project summary: In this project, we created a model of infectious virus spreading process. The simulation is all based on the model we built, it is just for visualizing the process of how the virus spread out in population, but not a future prediction.
  + We created a city with specified number of people in the city with higher density in the inner city and lower density in the outer side. In order to keep the population density during the random walk process, we built some invisible walls which has some possibility to send people back to the inner side of the wall.
  + We let some people in the largest city start to carry the virus. Each one has ability of being infected and protecting themselves and the probability of getting ease and worse are different based on the their health condition. After having these, the virus start to spread out.
  + Next step is to build some public places. We have restaurants, hospitals and transportation stations. For restaurants, people close to restaurant will have some probability of getting into the restaurant and there will be a higher probability of getting infected. The longer people staying in the restaurant, the higher probability they will come out of the restaurant. and higher probability of getting infected in the restaurant. 
  + For hospital, when people start to have the severe symptoms, they will be send to the hospital if there is still space in the hosipital. After getting into hospital, we let some of the people to ease their case and then send cured or dead people out of hospital. For station, it is used for city-wide exchange of people.
  + In our model, there are also some policies, like wearing mask, develop vaccines and close station. This policies will significantly affect the epidemic situation. If the policy is used earlier, the number of deaths can be greatly reduced.
  + Then we built specified number of cities with the same features and the stations will bring the virus from one city to others. 
  + Finally, we put the whole model in Shiny App. So that you can implement the policy, change the infectious ability on any day and save all the previous results as gif.
	
**Contribution statement**: 
  + Jinxu Xiang provided the idea of this project and built step 1 to step 9 and step 21 in Random_Walk.Rmd as start code. These steps allow citizen to be randomly walked and infected, enter and exit the restaurant, change citizen's condition and generate gifs based on many days of data.
  + Vikki Sui wrote step 10 to step 12, which is the hospital section. 
  + Ziqin Zhao built step 13 and 14. She provided all policies and produced code to track and quarantine close contacts. 
  + Junyan Guo's code made the station operational and proposed a multi-city model, which are step 15 and 16. 
  + Jinxu built step 17 which can clearly demonstrate the movement of people between cities, but unfortunately it is incompatible with the previous steps and cannot be used. 
  + Shuxin Chen provided the idea and code for step 18 and 19, added the chance for recrudescence and vaccine to cure disease. 
  + Everyone provided their own part in step 20 and together formed the simulation section.
  + Jinxu modified the entire model and put it into the global.r and built the UI and Server of Shiny App. 
  + Ziqin is the presenter and she used website form Shiny App to present.

**Presentation link:** 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
