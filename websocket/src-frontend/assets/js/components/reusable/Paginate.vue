<template>
  <div class="pagination">
    <div style="text-align: center;">
      <p>Страница {{ value }} из {{ lastPage }}</p>
      <div class="input-group" style="max-width: 170px; display: inline-table;">
        <span class="input-group-btn">
          <button class="btn btn-info btn-sm" type="button" :disabled="value <= 1" @click="setPage(1)"><<</button>
        </span>            
        <span class="input-group-btn">
          <button class="btn btn-info btn-sm" type="button" :disabled="value <= 1" @click="setPage(value-1)"><</button>
        </span>
        <input type="text" class="form-control input-sm" style="text-align: center;" :value="value" @keyup.13="setPage(Number($event.target.value))">
        <span class="input-group-btn">
          <button class="btn btn-info btn-sm" type="button" :disabled="value === lastPage" @click="setPage(value+1)">></button>
        </span>      
        <span class="input-group-btn">
          <button class="btn btn-info btn-sm" type="button" :disabled="value === lastPage" @click="setPage(lastPage)">>></button>
        </span>                    
      </div><!-- /input-group -->
    </div><!-- /.col-lg-6 -->
  </div><!-- /.row -->             
</template>

<script>
  export default {
    props: {
      lastPage: {
        type: Number,
        required: true,
        default: 1
      },
      value: {
        type: Number,
        required: false,
        default: 1
      }      
    },    
    methods:{
      setPage(val){
        if(val > this.lastPage) val = this.lastPage;
        if(val < 1) val = 1;
        this.$emit('input', val);
      }
    }
  }
</script>

<style>
  .pagination {
      margin-bottom: 0;
  }  
</style>