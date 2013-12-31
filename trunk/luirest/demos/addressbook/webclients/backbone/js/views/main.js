app.MainView = Backbone.View.extend({
  mainHtml: document.getElementById('main-template').innerHTML,
  initialize: function(){
    this.contactsView = new app.ContactListView({collection: app.contacts});
    this.phonesView = new app.PhoneListView({collection: app.phones})
    this.listenTo(app.contacts, 'contact:select', this.onContactSelect)
    this.listenTo(app.contacts, 'contact:edit', this.onContactEdit)
  },
  render: function(){
    this.$el.html(this.mainHtml);
    this.$('#contacts-table tbody').replaceWith(this.contactsView.render().$el)
    this.$('#phones-table tbody').replaceWith(this.phonesView.render().$el)
    return this;
    },
    //events
  events: {
    'click #add-contact': 'handleContactAdd'
  },
    onContactSelect: function(contact){
      if (contact !== app.phones.contact){
        app.phones.contact = contact;
        app.phones.fetch({reset: true})
      }
    },
    onContactEdit: function(contact){
      var dialog = new app.ContactDialogView({model: contact, collection: app.contacts, target: '#contact-dialog'});
      console.log('contactEdit model: ', JSON.stringify(contact.toJSON()) )
      dialog.render().show();
    },
    handleContactAdd: function(e){
      var dialog;
      var contact;
      e.preventDefault()
      e.stopPropagation()
      contact = new app.Contact()
      dialog = new app.ContactDialogView({model: contact, collection: app.contacts, target: '#contact-dialog'});
      console.log('contactAdd model: ', JSON.stringify(contact.toJSON()) )
      dialog.render().show();
    }
  }
)
