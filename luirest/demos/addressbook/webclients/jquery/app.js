//todo: make app a closure
function App(){
  var contactsTemplate,  phonesTemplate, contactDialogTemplate, phoneDialogTemplate;
  var baseURL = '../../luirest/addressbook.cgi/';
  var self = this;
  var contactsData = [];
  var phonesData = [];
  //methods
  this.init = function(){
    // properties
    contactsTemplate = Handlebars.compile(document.getElementById('contacts-template').innerHTML);
    contactDialogTemplate = Handlebars.compile(document.getElementById('contact-dialog-template').innerHTML);
    phonesTemplate = Handlebars.compile(document.getElementById('phones-template').innerHTML);
    phoneDialogTemplate = Handlebars.compile(document.getElementById('phone-dialog-template').innerHTML);

    //defaults
    $.ajaxSetup({
      dataType: 'json',
      contentType: 'application/json; charset=UTF-8',
      processData: false,
      error: function(jqXHR, textStatus){
        alert('Error while communicating to the server: ' + jqXHR.responseText);
      }
    });

    //wire events

    //setup modal dialog
    $('.dynamic-modal').on('hidden.bs.modal', function(){
      $(this).empty();
    }).on('show.bs.modal', function(){
      $(this).find('.modal-content').css('visibility','hidden');
    }).on('shown.bs.modal', function(){
      $(this).find('.modal-content').css({
        marginTop: function(index, oldValue){
            return ($(window).height() - $(this).height()) / 2 - parseInt($(this).css('padding-top'))
          },
        visibility: 'visible'
        }
      )
      });

    //contacts
    $('#contacts-table').on('click', 'tbody > tr', function(e){
      var id = e.currentTarget.dataset.contactId;
      $(e.currentTarget).addClass('active').siblings().removeClass('active');
      if (id) {
        self.updateContactPhones(id);
      }
    });

    $('#contact-dialog').on('click', '.save-data', function(e){
      var data = $(e.delegateTarget).find('form').serializeObject();
      var method = 'POST';
      var path = baseURL + 'contacts';
      var id = data.id;
      if (id) {
        //data already exists. Update it
        path += '/' + id;
        // force it to be a number
        data.id = parseFloat(id);
        method = 'PUT';
      };
      $.ajax(path, {
        type: method,
        data: JSON.stringify(data),
        success: function(responseData){
          self.updateContacts(id);
        }
      });
    });
    $('#add-contact').click(function(e){
      e.preventDefault();
      var context ={name:'New Contact'};
      var contents = contactDialogTemplate(context);

      $('#contact-dialog').html(contents).modal({
        backdrop: 'static'
      });
    });

    $('#contacts-table').on('click', 'a.edit-action', function(e){
      e.preventDefault();
      var id = $(this).parents('tr:first').data('contactId');
      var context;
      var contents;
      var $dialog = $('#contact-dialog');
      var data = $.grep(contactsData, function (item) {
        return item.id === id;
      })[0];
      if (data){
        context = data;
        contents = contactDialogTemplate(context);
        $dialog.html(contents);
        $dialog.modal({
          backdrop: 'static'
        });
      }
    });

    //phones

    $('#phone-dialog').on('click', '.save-data', function(e){
      var contactId = $(e.delegateTarget).find('.modal-dialog').data('contactId');
      var data = $(e.delegateTarget).find('form').serializeObject();
      var method = 'POST';
      var path = baseURL + 'contacts/' + contactId + '/phones';
      if (data.id) {
        //data already exists. Update it
        path += '/' + data.id;
        // force it to be a number
        data.id = parseFloat(data.id);
        method = 'PUT';
      };
      $.ajax(path, {
        type: method,
        data: JSON.stringify(data),
        success: function(responseData){
          self.updateContactPhones(contactId);
        }
      });
    });
    $('#add-phone').click(function(e){
      e.preventDefault();
      var context ={name:'New Phone'};
      context.contactId = $('#phones-table').data('contactId');
      var contents = phoneDialogTemplate(context);

      $('#phone-dialog').html(contents).modal({
        backdrop: 'static'
      });
    });

    $('#phones-table').on('click', 'a.edit-action', function(e){
      e.preventDefault();
      var id = $(this).parents('tr:first').data('phoneId');
      var context;
      var contents;
      var $dialog = $('#phone-dialog');
      var data = $.grep(phonesData, function (item) {
        return item.id === id;
      })[0];
      if (data){
        context = data;
        context.contactId = $(e.delegateTarget).data('contactId');
        contents = phoneDialogTemplate(context);
        $dialog.html(contents).modal({
          backdrop: 'static'
        });
      }
    });

    this.updateContacts();
  }  

  //methods

  this.updateContactsView = function(data){
    var context = {contacts: data};
    var contents = contactsTemplate(context);
    contactsData = data;
    $('#contacts-table').find('tbody').html(contents);
  };

  this.updateContactPhonesView = function(data){
    var context = {phones: data};
    var contents = phonesTemplate(context);
    phonesData = data;
    $('#phones-table').find('tbody').html(contents);
  };  

  this.updateContacts = function(activeId){
    $.get(baseURL + 'contacts', function(data)
    {
      self.updateContactsView(data);
      if (activeId){
        $('#contacts-table').find('tr[data-contact-id="' + activeId + '"]').trigger('click')
      } else
      {
        $('#contacts-table').find('tbody tr:first').trigger('click');
      }
    })
      .fail(function(){alert('Error loading contacts')});  
  };
  
  this.updateContactPhones = function(contactId){    
    $.get(baseURL + 'contacts/' + contactId + '/phones', function(data){
      $('#phones-table').data('contactId', contactId);
      self.updateContactPhonesView(data);
    })
      .fail(function(){alert('Error loading phones')});    
  }
};

var app = new App();

$(document).ready(function(){  
  app.init();
})