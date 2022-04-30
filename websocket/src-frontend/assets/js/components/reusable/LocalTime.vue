<template>
  <span class="entry-time">{{currTime}}</span>
</template>

<script>
    export default {
      props: {
        utcTime: {
          required: false,      
          default: null
        }
      },
      data(){
        return{
          currTime: null
        }
      },
      mounted(){
        if(this.utcTime && this.utcTime.length > 3){
          let dt = moment(moment(this.utcTime).utc(true).format()).local();
          this.currTime = dt.format('DD MMMM YYYY HH:mm');
        }
      },
    }
</script>

<style scoped>
.uploadbtn {
  margin: 10px auto;
  display: block;
}
</style>