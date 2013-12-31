var app = {};

app.BASE_URL = '../../luirest/addressbook.cgi';

$(document).ready(function(){
  //bootstrap
  app.phones = new app.PhoneList();
  app.contacts = new app.ContactList();
  app.contacts.fetch({
    reset: true,
    success: function(collection){
      console.log('fetch success ' + collection.length)
    },
    error: function(collection){
      console.log('fetch error')
    }
  });
  new app.AppView().render();
})