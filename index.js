
const { Pool } = require('pg');
const pool = new Pool({
  connectionString: '***',
  ssl: true,
  rejectUnauthorized: true
});
const express = require('express')
const app = express()
const port = 5000;
const path = require('path');
var bodyParser = require('body-parser')

app.use(bodyParser.json());

// Serving images
app.use(express.static(path.join(__dirname+'/htmls')));

app.get('/', (req, res) => 
  res.sendFile(path.join(__dirname+'/htmls/consent_screen.HTML')))

app.post('/server/consent', async (req, res) => {
    let id = req.body.user_id;
    let agree_button = req.body.agree_button;
    let user_group = req.body.filler_group;
    let origin_device = req.body.origin_device;
    var today = new Date();
    var date = today.getUTCFullYear()+'-'+(today.getUTCMonth()+1)+'-'+today.getUTCDate();
    var time = today.getUTCHours() + ":" + today.getUTCMinutes() + ":" + today.getUTCSeconds();
    var dateTime = date+' '+time;
    const client = await pool.connect();
    await client.query("insert into users (user_id, agree_button, filler_group, agree_button_time, origin_device) values ("+id+", "+agree_button+", '"+user_group+"', to_timestamp('"+dateTime+"', 'YYYY-MM-DD HH24:MI:SS'), '"+origin_device+"')"
    
      , (err, result) => {
        if (err != null){
          client.release();
          res.send(err);
        } else {
          client.release();
          res.send(result);
        }
      })
    
})

app.post('/server/get_anon_ids', async (req, res) => {
  const client = await pool.connect();
  await client.query("select user_id from users;"
    , (err, result) => {
      if (err != null){
        client.release();
        res.send(err);
      } else {
        client.release();
        res.send(result);
      }
    }) 
})

app.post('/server/verify_code', async (req, res) => {
  let id = req.body.user_id;
  const client = await pool.connect();
  await client.query("select user_id, filler_group from users where user_id = "+id+";"
  , (err,result) => {
    if (err!= null){
      client.release();
      res.send(err)
    }
    else {
      client.release();
        res.send(result);
    }
  })
})

app.post('/server/search_flight', async (req, res) => {
  let id = req.body.user_id;
  let experiment = req.body.experiment_device;
  const client = await pool.connect();
  await client.query("update users set experiment_device = '"+experiment+"' where user_id = "+id+";"
  , (err, result) => {
    if (err!= null){
      client.release();
      res.send(err);
    }
    else {
      client.release();
        res.send(result);
    }
  })
})

app.post('/server/load_flight_list', async (req, res) => {
  let id = req.body.user_id;
  let load_flight_list = req.body.load_flight_list;
  var today = new Date();
  var date = today.getUTCFullYear()+'-'+(today.getUTCMonth()+1)+'-'+today.getUTCDate();
  var time = today.getUTCHours() + ":" + today.getUTCMinutes() + ":" + today.getUTCSeconds();
  var flight_list_time = date+' '+time;
  const client = await pool.connect();
  await client.query("update users set load_flight_list = '"+load_flight_list+"', flight_list_time = to_timestamp('"+flight_list_time+"', 'YYYY-MM-DD HH24:MI:SS') where user_id = "+id+";"
  , (err, result) => {
    if (err!= null){
      client.release();
      res.send(err);
    }
    else {
      client.release();
        res.send(result);
    }
  })
})

app.post('/server/pwt_question', async (req, res) => {
  let id = req.body.user_id;
  var today = new Date();
  var date = today.getUTCFullYear()+'-'+(today.getUTCMonth()+1)+'-'+today.getUTCDate();
  var time = today.getUTCHours() + ":" + today.getUTCMinutes() + ":" + today.getUTCSeconds();
  var pwt_time = date+' '+time;
  const client = await pool.connect();
  await client.query("update users set fi1 = "+req.body.FI1+", fi2 = "
                      +req.body.FI2+", fi3 = "+req.body.FI3+", fi4 = "
                      +req.body.FI4+", td1 = "+req.body.TD1+", td2 = "
                      +req.body.TD2+", td3 = "+req.body.TD3+", td4 = "
                      +req.body.TD4+", td5 = "+req.body.TD5+", he1 = "
                      +req.body.HE1+", he2 = "+req.body.HE2+", he3 = "
                      +req.body.HE3+", he4 = "+req.body.HE4+", pwt1 = "
                      +req.body.PWT1+", pwt2 = "+req.body.PWT2+", pwt3 = "
                      +req.body.PWT3+", aa1 = "+req.body.AA1+", aa2 = "
                      +req.body.AA2+", aa3 = "+req.body.AA3+", aa4 = "
                      +req.body.AA4+", aa5 = "+req.body.AA5+", ca1 = "
                      +req.body.CA1+", ca2 = "+req.body.CA2+", ca3 = "
                      +req.body.CA3+", ca4 = "+req.body.CA4+", ui1 = "
                      +req.body.UI1+", ui2 = "+req.body.UI2+", ui3 = "
                      +req.body.UI3+", pwt_time = to_timestamp('"+pwt_time+"', 'YYYY-MM-DD HH24:MI:SS') where user_id = "+id+";"
  , (err, result) => {
    if (err!= null){
      client.release();
      res.send(err);
    }
    else {
      client.release();
        res.send(result);
    }
  })
})

app.post('/server/controls_question', async (req, res) => {
  let id = req.body.user_id;
  var today = new Date();
  var date = today.getUTCFullYear()+'-'+(today.getUTCMonth()+1)+'-'+today.getUTCDate();
  var time = today.getUTCHours() + ":" + today.getUTCMinutes() + ":" + today.getUTCSeconds();
  var controls_time = date+' '+time;
  const client = await pool.connect();
  await client.query("update users set gender = '"+req.body.gender+"', age_group = '"
                      +req.body.age_group+"', education_level = '"+req.body.education_level+"', device_hours = '"
                      +req.body.device_hours+"', device_flights = '"+req.body.device_flights+"', device_type = '"
                      +req.body.device_type+"', controls_time = to_timestamp('"+controls_time+"', 'YYYY-MM-DD HH24:MI:SS') where user_id = "+id+";"
  , (err, result) => {
    if (err!= null){
      client.release();
      res.send(err);
    }
    else {
      client.release();
        res.send(result);
    }
  })
})

app.post('/server/space_question', async (req, res) => {
  let id = req.body.user_id;
  var today = new Date();
  var date = today.getUTCFullYear()+'-'+(today.getUTCMonth()+1)+'-'+today.getUTCDate();
  var time = today.getUTCHours() + ":" + today.getUTCMinutes() + ":" + today.getUTCSeconds();
  var exit_time = date+' '+time;
  const client = await pool.connect();
  await client.query("update users set open_close_space = "+req.body.open_close_space+", private_public_space = "
                      +req.body.private_public+", large_small_space = "+req.body.large_small_space+", noisy_quiet_space = "
                      +req.body.Noisy_Quiet_space+", dark_bright_space = "+req.body.Dark_Bright_space+", crowded_space = "
                      +req.body.crowded_space+", exit_time = to_timestamp('"+exit_time+"', 'YYYY-MM-DD HH24:MI:SS') where user_id = "+id+";"
  , (err, result) => {
    if (err!= null){
      client.release();
      res.send(err);
    }
    else {
      client.release();
        res.send(result);
    }
  })
})

app.get('/controls_questions', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/controls_questions.HTML')))

app.get('/disagree_participation', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/disagree_participation.HTML')))

app.get('/flight_list', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/flight_list.HTML'))) 

app.get('/order_flight', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/order_flight.HTML')))

app.get('/mobile_enter_screen', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/mobile_enter_screen.HTML')))

app.get('/pc_enter_screen', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/pc_enter_screen.HTML')))  

app.get('/PWT_questions', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/PWT_questions.HTML')))

app.get('/PWT2-NoFiller-questions', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/PWT2-NoFiller-questions.HTML')))

app.get('/Space_questions', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/Space_questions.HTML')))

app.get('/switch_to_mobile', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/switch_to_mobile.HTML')))

app.get('/switch_to_pc', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/switch_to_pc.HTML')))  

app.get('/ThankYou', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/ThankYou.HTML')))

app.get('/Waiting_Inter_and_Rev', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/Waiting_Inter_and_Rev.HTML')))

app.get('/Waiting_Inter_and_NonRev', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/Waiting_Inter_and_NonRev.HTML')))

app.get('/Waitng_NonInter_and_NonRev', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/Waitng_NonInter_and_NonRev.HTML')))

app.get('/Waitng_NonInter_and_Rev', (req,res) => 
  res.sendFile(path.join(__dirname+'/htmls/Waitng_NonInter_and_Rev.HTML')))

app.listen(process.env.PORT || port, function (){
  console.log(`Example app listening on port ${port}!`)
})