# Project: Words 4 Music

### [Project Description](doc/Project4_desc.md)

![image](http://cdn.newsapi.com.au/image/v1/f7131c018870330120dbe4b73bb7695c?width=650)

Term: Fall 2016

+ [Data link](https://courseworks2.columbia.edu/courses/11849/files/folder/Project_Files?preview=763391)-(**courseworks login required**)
+ [Data description](doc/readme.html)
+ Contributor's name: Lily Jiang
+ Projec title: Association mining of music and text
+ Project summary: For this project, set of features and lyrics of songs are given and we are trying to mine the association between the features and lyrics and thus predict the possible words which may appear in a song based on its features. 

The main scripts are inclued in main.R. At first labels are assigned to songs based on Topic Modelling based on its lyrics. Then features of songs are processed for use and several Multi Class Classification ML methods are applied to build the model between features and topic labels. Then for test songs they can be classified and given the word rank based on its topics.
	
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
