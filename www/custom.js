// see https://shiny.rstudio.com/articles/js-events.html

$(document).on('shiny:connected', function() {
  console.log('connected');
    
  // see https://shiny.rstudio.com/articles/communicating-with-js.html
  Shiny.setInputValue('user_start', + new Date());
});


$(document).on('shiny:disconnected', function() {
  console.log('disconnected');
  
  Shiny.setInputValue('user_end', + new Date());
});


$(window).on('beforeunload', function() { 
  console.log('closed');
  
  Shiny.setInputValue('user_end', + new Date());
})
