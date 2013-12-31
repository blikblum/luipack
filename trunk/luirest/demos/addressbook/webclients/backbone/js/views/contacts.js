app.ContactListView = Backbone.View.extend({
  tagName: 'tbody',
  initialize: function(){
    this.listenTo(this.collection, 'reset', this.render);
    this.listenTo(this.collection, 'add', this.renderContact);
  },
  render: function(){
    this.collection.each(this.renderContact, this);
    return this;
  },
  renderContact: function(contact){
    this.$el.append(new app.ContactView({model: contact}).render().$el)
  },
  //events
  events:{
    'click tr a.edit': 'onEditClick'
  },
  onEditClick: function(e){

  }

})