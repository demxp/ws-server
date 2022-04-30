<template>
      <div class="box">
        <div class="box-header with-border">
          <h3 class="box-title" v-text="mode.box_title"></h3>
        </div>
        <div class="box-body">
          <div class="row">
            <div class="col-md-6">
              <div class="form-group">
                <label for="exampleInputEmail1">Логин</label>
                <input type="text" class="form-control" v-model="user.login">
              </div>
              <div class="form-group">
                <label for="exampleInputEmail1">Имя</label>
                <input type="text" class="form-control" v-model="user.name">
              </div>
              <div class="form-group">
                <label for="exampleInputEmail1">Роль</label>
                <multiselect v-model="user.role" :options="allowed_roles" :searchable="false" :close-on-select="true" :show-labels="false" placeholder="Выберите роль" track-by="title" label="title" :disabled="user.main"></multiselect>
              </div>              
              <div class="form-group">
                <label for="exampleInputEmail1">Пароль</label>
                <input type="text" class="form-control" v-model="user.password" autocomplete="new-password">
              </div>
            </div>
          </div>
        </div>
        <div class="box-footer">
          <a class="btn btn-default" @click="$parent.$emit('switch-mode', {'mode': 'users', 'item': null})">Назад</a>
          <button class="btn pull-right" :class="{'btn-success':mode.submit_style_success, 'btn-warning':mode.submit_style_warning}" @click="edituser" v-text="mode.submit_text"></button>
        </div>
      </div>
</template>

<style src="vue-multiselect/dist/vue-multiselect.min.css"></style>

<script>
    import Multiselect from 'vue-multiselect';

    export default {
      components: {Multiselect},      
      props: {
        userSet: {
          required: false,      
          default: null
        }
      },
      data(){
        return{
          user: {
            id: null,
            login: null,
            name: null,
            role: {title: 'Пользователь', id: 'ordinar'},
            password: null,
            old_password: '',
            main: false
          },
          mode:{
            box_title: "Добавляем пользователя",
            submit_text: "Добавить",
            submit_style_success: true,
            submit_style_warning: false
          },
          allowed_roles: [
            {title: 'Пользователь', id: 'ordinar'},
            {title: 'Модератор', id: 'moderator'},
            {title: 'Администратор', id: 'admin'}
          ]
        }
      },
      mounted(){
        if(!!this.userSet){
          this.mode = {
            box_title: "Изменяем пользователя",
            submit_text: "Изменить",
            submit_style_success: false,
            submit_style_warning: true
          };
          for(let i in this.userSet){
            if(i == 'role'){
              let rl = this.userSet.role;
              this.user.role = this.allowed_roles.find(function(item, index, array) {
                return item.id == rl;
              });
            }else{
              this.user[i] = this.userSet[i];
            }
          }
        }
      },
      methods:{
        edituser(){
          if(this.user.login === null || this.user.login.length < 3){
            customAlert({text: "Надо написать логин пользователя!"});
            return false;
          }      
          if(this.user.name === null || this.user.name.length < 3){
            customAlert({text: "Надо написать имя пользователя!"});
            return false;
          }
          if(this.user.password === null || this.user.password.length < 3){
            customAlert({text: "Надо написать пароль пользователя!"});
            return false;
          }

          this.$root.wssend({
            action: (!!this.user.id) ? 'update_user' : 'create_user',
            id: this.user.id,
            login: this.user.login,
            name: this.user.name,
            role: this.user.role.id,
            password: this.user.password,
            old_password: this.user.old_password
          }, function(d){
            if(d.status == 'ok'){
              this.$parent.$emit('switch-mode', {'mode': 'users', 'id': null})
            }else if('error' in d && d.error == 'badpassword'){
              this.$parent.$emit('switch-mode', {'mode': 'users', 'id': null});
              customAlert({text: "Неверный пароль системной учетной записи. Отмена."});              
            }else{
              console.error("USER CREATE ERROR", d);
              customAlert({text: "Какая-то ошибка!"});
            }
          }.bind(this), true);
        }
      }    
    }
</script>