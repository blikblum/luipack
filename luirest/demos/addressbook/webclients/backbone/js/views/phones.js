app.PhoneListView = Backbone.View.extend({
  tagName: 'tbody',
  initialize: function(){
    this.listenTo(this.collection, 'reset', this.render);
  },
  render: function(){
    this.$el.empty()
    this.collection.each(function(phone){
      this.$el.append(
        new app.PhoneView({model: phone}).render().$el
      )
    }, this);
    return this;
  }

})