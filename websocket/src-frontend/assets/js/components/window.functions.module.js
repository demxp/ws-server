export function WindowInstallCustom() {
  window.customAlert = (resp) => {
    let text = '';
    let mode = false;
    let errors = {
      'IncorrectInputData': ((resp) => "Некорректные данные"),
      'NotEnoughRights': ((resp) => "Не хватает прав для выполнения"),      
      'ValidateError': ((resp) => {
        return resp.errors.reduce((acc, item) => {
          return acc += item + '\n';
        }, "");
      })
    };

    if (!!errors[resp.message]) {
      text = errors[resp.message](resp);
    } else if (!!resp.text) {
      text = resp.text;
      mode = ('mode' in resp) ? resp.mode : false;
    } else if (!!resp.message) {
      text = resp.message;
    } else if (!!resp) {
      text = "Неизвестная ошибка!";
      console.log(resp);
    }

    if(mode){
      swal('', text, mode);
    }else{
      swal("Oops!", text, "error");
    }
    return;
  };
  window.moment = require('moment');
  window.moment.locale('ru');
}