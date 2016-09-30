### Description  
This app was made to show the most probable next word according to the trainning dataset.  
The dataset, used to train the NGRAM model, is from SwiftKey. You can download the data (here)[https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip]

### Metodology   
This model was made in order to complete the data science specialization provided by John Hopkins trough Coursera. More information about the data science specialization can be found (here)[https://en.coursera.org/specializations/jhu-data-science]  
The model is a simple (NGRAM)[https://en.wikipedia.org/wiki/N-gram], from level 1 to level 4, that calculates the probabilities, and conditional probabilities too, using (Katz Backoff Strategy)[https://en.wikipedia.org/wiki/Katz%27s_back-off_model] and (Add-1 smooth - LAPLACE)[https://en.wikipedia.org/wiki/Additive_smoothing]

### How to use  
Type any english word, in the indicated place, and check the most probable word, considering the context that the user inserted.  
The user can manage how many words he/she wants to see, to do so slide the bar and change the values from 1 to 10.  

### More information  
The full code can be found (here)[https://github.com/skineer/Coursera_Final_Project]  
The code was constructed by Renato Pedroso in Sep, 30th - 2016  
