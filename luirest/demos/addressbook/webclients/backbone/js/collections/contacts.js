var app = app || {};

app.ContactList = Backbone.Collection.extend({
  url: app.BASE_URL + '/contacts',
  model: app.Contact
})