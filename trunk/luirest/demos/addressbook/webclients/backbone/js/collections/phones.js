app.PhoneList = Backbone.Collection.extend(
  {
    url: function(){
      return app.BASE_URL + '/contacts/' + this.contact.get('id') + '/phones'
    },
    model: app.Phone
  }
)