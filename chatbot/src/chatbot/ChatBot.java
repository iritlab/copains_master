/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package chatbot;

/**
 *
 * @author jorge
 */

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.font.FontRenderContext;
import java.awt.font.LineBreakMeasurer;
import java.awt.font.TextAttribute;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.lang.Math;
import java.net.URL;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

public class ChatBot extends JFrame implements KeyListener{

    
	JPanel p=new JPanel();
        JButton resetButton = new JButton("reset");
        JButton playButton = new JButton("Play");
   
        
	//JTextArea dialog=new JTextArea(20,60);
        
        JTextPane dialog = new JTextPane();
        StyledDocument doc = dialog.getStyledDocument();
        
	JTextArea input=new JTextArea(0,59);
        
	JScrollPane scroll=new JScrollPane(
		dialog,
		JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
		JScrollPane.HORIZONTAL_SCROLLBAR_NEVER
	);
        
        
	JScrollPane scroll2=new JScrollPane(
		input,
		JScrollPane.VERTICAL_SCROLLBAR_NEVER,
		JScrollPane.HORIZONTAL_SCROLLBAR_NEVER
	);
        
	
	String[][] chatBot={
		//standard greetings
		{"hi","hello","hola","ola","howdy"},
                {"hi, would you like to set the variables ?"},
                
		{"yes"},
                {"call_yes"},                
		//{"call_individual"},                
                
                
                
		//question greetings
		{"how are you","how r you","how r u","how are u"},
		{"good","doing well"},
		//yes
		{"no so good","I feel pain"},
		{"do you take the medicine ?"},
                
                /*
		{"no", "no yet"},
		{"why ? Options : 1. Feel pain / 2. Have fever"},
                */
		//default

		{"1"},
		{"Do you think the medicine produce the pain ?"},
                

                
		{"no"},
                {"call_no"},
		//{"Would you like to setup a conditional desire ?"},                
		//default                
                
		//{"yes","ok"},
		//{"call_conditional"},                  
                
		{"finish"},
		{"call_plan"},                  
                
                
                
		{"shut up","you're bad","noob","have you taken your medicine ?", "stop talking",
		"(michael is unavailable, due to LOL)"}
	};
	
	public static void main(String[] args) {
		new ChatBot();
	}
    File file;
	
	public ChatBot(){
		super("Chat Bot");
		setSize(700,400);
                
		//setResizable(false);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		
		dialog.setEditable(true);
		input.addKeyListener(this);
                scroll.setPreferredSize(new Dimension(650, 300));              
	
		p.add(scroll);
                p.add(scroll2);
		//p.add(input);

		p.setBackground(new Color(255,200,0));
		add(p);
		setVisible(true);

                        String st;
                        st = "Hi! How are you?, I am Botty your personal assistant";
                        addText2(" Agent : \t"+st,1);     
                        st = "I will help you to find the best sport based in your preferences";
                        addText2("\n Agent : \t"+st,1);                         
                        st = "Are you ready to start ?";
                        addText2("\n Agent : \t"+st,1);                                                 
                        
                        input.setText(null);
                        input.grabFocus();
                        
                        File f= new File("/home/jorge/sw/code/chatbot/solvedatacp/plan_final_show.txt");   //file to be delete  
                        //File f2= new File("/home/jorge/sw/code/sdata/00_delta0.txt");   //file to be delete  
                        File f3= new File("/home/jorge/sw/code/sdata/00_delta0_base.txt");   //file to be delete                          
                        
                        f.delete();
                        f3.delete();
                        
                        try {
                            File file = new File("/home/jorge/sw/code/sdata/00_delta0_base.txt");
                            file.createNewFile();
                            System.out.println("Empty File Created:- " + file.length());
                        } catch (IOException e) {
                            e.printStackTrace();
                        }

                        
                        
                        /*
                        if(f.delete())                      
                        {  
                            
                            System.out.println(f.getName() + " and"+ f2.getName()+" deleted");
                        }  
                        else  
                        {  
                            System.out.println("failed");  
                        } 
                        */
	}
	
	public void keyPressed(KeyEvent e){
		if(e.getKeyCode()==KeyEvent.VK_ENTER){
			input.setEditable(false);
                        
                        
			String quote=input.getText();
			input.setText("");
			addText2("\n"+quote.trim()+"\t : Human ",2);
			while(
				quote.charAt(quote.length()-1)=='!' ||
				quote.charAt(quote.length()-1)=='.' ||
				quote.charAt(quote.length()-1)=='?'
			){
				quote=quote.substring(0,quote.length()-1);
			}
			
                        //System.out.println ("Human input :: #"+ quote+"#");
                        //System.out.println ("Human input :: @"+ quote.trim()+"@");
                        
			byte response=0;
			//-----check for matches----
			int j=0;//which group we're checking
			while(response==0){
				if(inArray(quote.trim().toLowerCase(),chatBot[j*2])){
                                    try {
                                        response=2;
                                        int r=(int)Math.floor(Math.random()*chatBot[(j*2)+1].length);
                                        String s = chatBot[(j*2)+1][r];
                                        
                                        
                                        int nl = countLines(dialog);
                                        String lq = "";
                                        System.out.println ("Numero de Linea :: \n"+nl);
                                        lq = getLine(dialog, nl - 1);
                                        //System.out.println ("Last Question   :: \n"+ lq);
                                        
                                        
                                        
                                        //if ("call_yes".equals(s) && (" Agent : 	Would you like to setup a conditional desire ?".equals(lq.trim()) || " Agent : 	hi, would you like to set the variables ?".equals(lq.trim()) )) {
                                        if ("call_yes".equals(s) && ("Agent : 	Do you want to continue adding more individual desires ?".equals(lq.trim()) || "Agent : 	hi, would you like to set the variables ?".equals(lq.trim()) || "Agent : 	Are you ready to choose your preferences ?".equals(lq.trim()) )) {
                                            
                                            try {
                                                MyDialog1 dlg = new MyDialog1(this);
                                                String[] results = dlg.run();
                                                /*if (results[0] != null) {
                                                JOptionPane.showMessageDialog(this,
                                                "Variable = "+ results[1] + ", Value = " + results[0]);
                                                }*/
                                                
                                                String st;
                                                st = "You selected : ";
                                                addText2("\n Agent : \t"+st,1);
                                                
                                                st = results[1] +" = " + results[0];
                                                addText2(st,3);
                                                
                                                /*************/
                                                String assigment = "";
                                                String assigmentx = "";
                                                
                                                if (results[0].indexOf("not") >= 0) {
                                                    results[0] = results[0].replace("not ","");
                                                    assigment = "not ass("+results[1]+","+results[0]+")";
                                                    assigmentx = "not x_"+results[1]+"_"+results[0]+"_x";
                                                } else {
                                                    assigment = "ass("+results[1]+","+results[0]+")";
                                                    assigmentx = "x_"+results[1]+"_"+results[0]+"_x";
                                                }
                                                
                                                
                                                
                                                
                                                ////////////////////////
                                                //////////////////////// @@@
                                                
                                                saveInput(assigmentx);
                                                
                                                
                                                
                                                
                                                //System.out.println ("Insert into /sdata/00_delta0.txt :: "+assigment);
                                                /***********/
                                                
                                                st = "Do you want to continue adding more individual desires ?";
                                                addText2("\n Agent : \t"+st,1);
                                                
                                                input.setEditable(true);
                                                
                                            } catch (Exception ex) {
                                                Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                                            }
                                        } //else if (s == "call_conditional") {
                                        else if ("call_yes".equals(s) && "Agent : 	Would you like to setup a conditional desire ?".equals(lq.trim())) {                                         
                                            try {
                                                MyDialog2 dlg = new MyDialog2(this);
                                                String[] results = dlg.run();
                                                /*if (results[0] != null) {
                                                JOptionPane.showMessageDialog(this,
                                                "Variable = "+ results[1] + ", Value = " + results[0]+"\n"+
                                                "Variable = "+ results[3] + ", Value = " + results[2]        );
                                                }*/
                                                
                                                String st;
                                                st = "You selected : ";
                                                addText2("\n Agent : \t"+st,1);
                                                
                                                st = results[1] +" = " + results[0]+"=>"+ results[3] +" = " + results[2];
                                                addText2(st,3);
                                                
                                                /*************/
                                                String assigment = "";
                                                String assigmentx = "";
                                                
                                                if (results[0].indexOf("not") >= 0) {
                                                    assigment = "not ass("+results[1]+","+results[0]+")";
                                                    assigmentx = "not x_"+results[1]+"_"+results[0]+"_x";
                                                    
                                                } else {
                                                    assigment = "ass("+results[1]+","+results[0]+") => ass("+results[3]+","+results[2]+")";
                                                    assigmentx = "x_"+results[1]+"_"+results[0]+"_x => x_"+results[3]+"_"+results[2]+"_x";
                                                    
                                                }
                                                
                                                saveInput(assigmentx);
                                                
                                                /*System.out.println ("Insert into 00_HUMAN :: "+assigment);*/
                                                System.out.println ("Insert into /sdata/00_delta0.txt :: "+assigment);
                                                /***********/
                                                
                                                st = "Would you like to setup a conditional desire ?";
                                                addText2("\n Agent : \t"+st,1);
                                                
                                                input.setEditable(true);
                                            } catch (Exception ex) {
                                                Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                                            }
                                            
                                            
                                        }  else if ("call_no".equals(s) && "Agent : 	Do you want to continue adding more individual desires ?".equals(lq.trim())) {                                               
                                            
                                            String st;
                                            st = "Would you like to setup a conditional desire ?";
                                            addText2("\n Agent : \t"+st,1);
                                            
                                        }   else if ("call_no".equals(s) && "Agent : 	Would you like to setup a conditional desire ?".equals(lq.trim())) {                                         
                                            
                                            try {
                                                String st;
                                                st = "Ok, these are your preferences : ";
                                                addText2("\n Agent : \t"+st,1);
                                                ExeProc("pro_show_selections");
                                                st = "Would you like me to find the best sport for you ?";
                                                addText2("\n Agent : \t"+st,1);                                                
                                                
                                            } catch (Exception ex) {
                                                Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                                            }
                                        } else if ("call_yes".equals(s) && "Agent : 	Would you like me to find the best sport for you ?".equals(lq.trim())) {
                                            
                                            try {
                                                String st;
                                                st = "Please wait, I am trying to find the best sport for You ";
                                                addText2("\n Agent : \t"+st,1);
                                                addText2("\n\t",1);
                                                ExeProc("pro_cog_plan");
                                            } catch (Exception ex) {
                                                Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                                            }
                                            
                                        } else if ("call_yes".equals(s) && "Agent : 	Are you ready to start ?".equals(lq.trim())) {
                                            
                                            String st;
                                            st = "Great !, let's start by selecting your desires.";
                                            addText2("\n Agent : \t"+st,1);
                                            st = "Are you ready to choose your preferences ?";
                                            addText2("\n Agent : \t"+st,1);                                            
                                            
                                        } else {
                                            addText2("\n Agent : \t"+s,1);
                                        }
                                    } catch (BadLocationException ex) {
                                        Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                                    }
                                    
				}
				j++;
				if(j*2==chatBot.length-1 && response==0){
					response=1;
				}
			}
			
			//-----default--------------
			if(response==1){
				int r=(int)Math.floor(Math.random()*chatBot[chatBot.length-1].length);
				addText("\n Agent : \t"+chatBot[chatBot.length-1][r]);
 			}
			//addText("\n");
		}
                
	}
	
	public void keyReleased(KeyEvent e){
		if(e.getKeyCode()==KeyEvent.VK_ENTER){
			input.setEditable(true);
		}
	}
	
	public void keyTyped(KeyEvent e){}
	
	public void addText(String str){
		dialog.setText(dialog.getText()+str);
	}

        public void addText2(String txt, int nColor) {
            SwingUtilities.invokeLater(new Runnable() {
               public void run() {
                   if (nColor == 1) {
                       try {
                           //dialog.setText(dialog.getText() + txt);
                           //dialog.update(dialog.getGraphics());
                           SimpleAttributeSet left = new SimpleAttributeSet();
                           StyleConstants.setAlignment(left, StyleConstants.ALIGN_LEFT);
                           StyleConstants.setForeground(left, Color.RED);
                           doc.insertString(doc.getLength(), txt, left );
                           doc.setParagraphAttributes(doc.getLength(), 1, left, false);
                       } catch (BadLocationException ex) {
                           Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                       }
                   } else if (nColor == 2) {
                       try {
                           //dialog.setText(dialog.getText() + txt);
                           //dialog.update(dialog.getGraphics());
                           SimpleAttributeSet right = new SimpleAttributeSet();
                           StyleConstants.setAlignment(right, StyleConstants.ALIGN_RIGHT);
                           StyleConstants.setForeground(right, Color.BLUE);
                           doc.insertString(doc.getLength(), txt, right );
                           doc.setParagraphAttributes(doc.getLength(), 1, right, false);
                       } catch (BadLocationException ex) {
                           Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                       }
                   } else if (nColor == 3) {
                       try {
                           //dialog.setText(dialog.getText() + txt);
                           //dialog.update(dialog.getGraphics());
                           SimpleAttributeSet left = new SimpleAttributeSet();
                           StyleConstants.setAlignment(left, StyleConstants.ALIGN_LEFT);
                           StyleConstants.setForeground(left, Color.BLACK);
                           doc.insertString(doc.getLength(), txt, left );
                           doc.setParagraphAttributes(doc.getLength(), 1, left, false);
                       } catch (BadLocationException ex) {
                           Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                       }
                   }
                  
                  
               }
            });
          }
        
        
        public String ReadFromFile2(String fileName, int rev) throws IOException {
            StringBuilder everything = new StringBuilder();
            String line;
            String[] linex;
            linex = new String[10];
            String preText = null;
            int i=0;
            
            //File file = new File("/home/jorge/sw/code/chatbot/solvedatacp/plan_final_show.txt");
            File file = new File(fileName);
            BufferedReader buffIn = new BufferedReader(new FileReader(file)); 
                
            if (rev == 1) {
                while( (line = buffIn.readLine()) != null) {
                   i ++;     
                   linex[i] = line;
                }

                while( i > 0) {
                   everything.append(linex[i]);
                   everything.append("\n\t");
                   i --; 
                }            

                //preText = "This is the best plan for you : \n\t";
                preText = "";
            } else if (rev == 0) {
                while( (line = buffIn.readLine()) != null) {
                   i ++;     
                   linex[i] = line;
                   everything.append(linex[i]);
                   everything.append("\n\t");
                }            
            
                preText = "";
            }
            return preText +"\n\t"+everything.toString();
        }        
        
        public void ExeProc(String plan) throws Exception  {  
    
        String s;
        Process p;
        try {
            
            switch (plan) {
                case "pro1":
                    /*/home/jorge/sw/full_expo*/
                    /*p = Runtime.getRuntime().exec("/home/jorge/sw/code/brev/brev -e s si1.txt");*/
                    /*p = Runtime.getRuntime().exec("/home/jorge/sw/code/brev/brev -e s si1.txt");*/


/*
                    p = Runtime.getRuntime().exec("/home/jorge/sw/code/brev/brev -e s score.txt svol.txt sinput1.txt");
                    BufferedReader br1 = new BufferedReader(
                        new InputStreamReader(p.getInputStream()));
                    while ((s = br1.readLine()) != null)
                        System.out.println("line: " + s);
                    p.waitFor();
                    System.out.println ("exit: " + p.exitValue());
                    p.destroy();   */
                    
                    /*
                    String var;
                    var = setVars();
                    System.out.println("The popup return ::: "+var);
                    */
                    
                    MyDialog dlg = new MyDialog(this);
                    String[] results = dlg.run();
                    if (results[0] != null) {
                       JOptionPane.showMessageDialog(this,
                          results[0] + ", color: " + results[1]);
                    }                    
                    
                    
                    
                    
                    
                    break;
                    


                case "pro_brev":
                    /*/home/jorge/sw/full_expo*/
                    /*p = Runtime.getRuntime().exec("/home/jorge/sw/code/brev/brev -e s si1.txt");*/
                    /*p = Runtime.getRuntime().exec("/home/jorge/sw/code/brev/brev -e s si1.txt");*/
                    p = Runtime.getRuntime().exec("/home/jorge/sw/code/brev/brev -e s score.txt 00_delta0_base.txt 04_input.txt");
                    BufferedReader br2b = new BufferedReader(
                        new InputStreamReader(p.getInputStream()));
                    while ((s = br2b.readLine()) != null)
                        System.out.println("line: " + s);
                    p.waitFor();
                    System.out.println ("exit: " + p.exitValue());
                    p.destroy();   
                    break;                    
                    
                case "pro_show_selections":
                    
                    String st="";
                    //File needed = new java.io.File("/home/jorge/sw/code/sdata/00_delta0.txt");
                    st = ReadFromFile2("/home/jorge/sw/code/sdata/00_delta0.txt",0);
                    addText2("\t"+st,3);                  
                    break;                         
                    
                case "pro2":
                    /*/home/jorge/sw/full_expo*/
                    /*p = Runtime.getRuntime().exec("/home/jorge/sw/code/brev/brev -e s si1.txt");*/
                    /*p = Runtime.getRuntime().exec("/home/jorge/sw/code/brev/brev -e s si1.txt");*/
                    p = Runtime.getRuntime().exec("/home/jorge/sw/code/brev/brev -e s score.txt svol.txt sinput2.txt");
                    BufferedReader br2 = new BufferedReader(
                        new InputStreamReader(p.getInputStream()));
                    while ((s = br2.readLine()) != null)
                        System.out.println("line: " + s);
                    p.waitFor();
                    System.out.println ("exit: " + p.exitValue());
                    p.destroy();   
                    break;
                    
                case "pro3":
                    /*/home/jorge/sw/full_expo*/
                    /*p = Runtime.getRuntime().exec("/home/jorge/sw/code/brev/brev -e s si1.txt");*/
                    /*p = Runtime.getRuntime().exec("/home/jorge/sw/code/brev/brev -e s si1.txt");*/
                    p = Runtime.getRuntime().exec("/home/jorge/sw/code/brev/brev -e s score.txt svol.txt sinput3.txt");
                    BufferedReader br3 = new BufferedReader(
                        new InputStreamReader(p.getInputStream()));
                    while ((s = br3.readLine()) != null)
                        System.out.println("line: " + s);
                    p.waitFor();
                    System.out.println ("exit: " + p.exitValue());
                    p.destroy();                       
                    break;
                    
                case "pro_cog_plan":    
                    
                    /*p = Runtime.getRuntime().exec("/home/jorge/sw/code/planer/planer -e s x_model.txt 01_actions.txt 01_goal.txt");*/
                    /*p = Runtime.getRuntime().exec("/home/jorge/sw/code/planer/planer -e s x_model.txt 01_actions.txt 01_goal.txt");*/
                    
                    p = Runtime.getRuntime().exec("/home/jorge/sw/code/translations/_build/default/src/main.exe --solve /home/jorge/sw/code/sdata/00_delta0.txt");
                    
                    BufferedReader br4 = new BufferedReader(
                        new InputStreamReader(p.getInputStream()));
                    while ((s = br4.readLine()) != null)
                        System.out.println("line: " + s);
                    p.waitFor();
                    System.out.println ("exit: " + p.exitValue());
                    p.destroy();
                    

                    
                    
                    Thread t1 = new Thread(new Runnable() {
                        Process p2;
                        String st=".";
                        String s2="";
                        long startTime, endTime, duration;
                        @Override
                        public void run() {
                            try {
                                // code goes here.
                                startTime = System.currentTimeMillis();
                                p2 = Runtime.getRuntime().exec("/home/jorge/sw/code/planer/planer -e s 01_ini_state.txt 02_actions.txt 03_goal.txt");
                                
                            } catch (IOException ex) {
                                Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                            }

                        BufferedReader br4 = new BufferedReader(
                            new InputStreamReader(p2.getInputStream()));
                            try {
                                while ((s2 = br4.readLine()) != null) {
                                    System.out.println("line: " + s2);
                                    /*addText2(st);*/
                                }
                            } catch (IOException ex) {
                                Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                            }
                            try {
                                p2.waitFor();
                            } catch (InterruptedException ex) {
                                Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                            }
                        endTime = System.currentTimeMillis();    
                        duration = (endTime - startTime);    
                        System.out.println ("Total time : " + duration);    
                        System.out.println ("exit: " + p2.exitValue());
                        p2.destroy();                            
                        }
                    });  
                    t1.start();                  
                    
/**************************/
                    
                    Thread t2 = new Thread(new Runnable() {
                        String s2="";
                        String st="+";
                        
                        File needed = new java.io.File("/home/jorge/sw/code/chatbot/solvedatacp/plan_final_show.txt");
                        
                        @Override
                        public void run() {
                            try {
                                // code goes here.
                                    while(!needed.exists()){
                                      Thread.sleep(500); //sleep for 2 seconds.. MUST DO THIS
                                      System.out.println ("Sleeping: ");
                                      //addText2("\n",1);
                                      addText2(st,1);
                                    }  
                                    
                                    System.out.println ("Fuera del WHILE ::: ");
                                    st = ReadFromFile2("/home/jorge/sw/code/chatbot/solvedatacp/plan_final_show.txt",1);

                                    String ideal = st.replace("ideal_te", "I recommend Tennis as the ideal sport for you because : ");
                                    String reason1 = ideal.replace("env_land", "environment is land");
                                    String reason2 = reason1.replace("intens_med", "intensity is medium");
                                    String reason3 = reason2.replace("loc_mixed", "location is mixed");
                                    String reason4 = reason3.replace("soc_mixed", "sociality is mixed");
                                    String plan = reason4;
                                    addText2("\n Agent : \t"+plan,1);
                                    
                                    
                            } catch (InterruptedException ex) {
                                Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                            } catch (IOException ex) {
                                Logger.getLogger(ChatBot.class.getName()).log(Level.SEVERE, null, ex);
                            }

                        }
                    });  
                    t2.start();                      
                    
                    
                    
/*
                    java.io.File needed;
                    needed = new java.io.File("/home/jorge/sw/code/chatbot/solvedatacp/final_plan2.txt");

                    while( !needed.exists() ){
                      Thread.sleep(2000); //sleep for 2 seconds.. MUST DO THIS
                      System.out.println ("Sleeping: ");
                    }                   
 */                   
                    
                    

                    break;
                    
                /*    
                case "pro_save_input":

                    p = Runtime.getRuntime().exec("cat "+assigment +"> /home/jorge/sw/code/sdata/04_input2.txt");
                    BufferedReader br5 = new BufferedReader(
                        new InputStreamReader(p.getInputStream()));
                    while ((s = br5.readLine()) != null)
                        System.out.println("line: " + s);
                    p.waitFor();
                    System.out.println ("exit: " + p.exitValue());
                    p.destroy();   
                    break;
*/
                    
                case "pro6":
                    
                    break;
                default: 
                    
                    break;
                }            
            
            
        } catch (Exception e) {}
    
}

        
	public boolean inArray(String in,String[] str){
		boolean match=false;
		for(int i=0;i<str.length;i++){
			if(str[i].equals(in)){
				match=true;
			}
		}
		return match;
	}
/*
    private String setVars() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
*/
        
        
/*
        private static int countLines(JTextArea textArea) {
          AttributedString text = new AttributedString(textArea.getText());
          text.addAttribute(TextAttribute.FONT, f);
          FontRenderContext frc = textArea.getFontMetrics(textArea.getFont()).getFontRenderContext();
          AttributedCharacterIterator charIt = text.getIterator();
          LineBreakMeasurer lineMeasurer = new LineBreakMeasurer(charIt, frc);
          float formatWidth = (float) textArea.getSize().width;
          lineMeasurer.setPosition(charIt.getBeginIndex());

          int noLines = 0;
          System.out.println ("COUNT LINES \n: "+lineMeasurer.getPosition());
          
          while (lineMeasurer.getPosition() < charIt.getEndIndex()) {
            lineMeasurer.nextLayout(formatWidth);
            noLines++;
          }

          return noLines;
        }        
*/


     
        
        
        
        
        
        public String setVars() throws Exception  {
         String desc1="";
         
                    try {
                        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                    } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException ex) {
                    }

                    JFrame f = new JFrame("Lister v1.0");
                    f.setSize(300, 300);
                    f.setLocation(300, 300);
                    f.addWindowListener(new WindowAdapter( ) {
                      @Override
                      public void windowClosing(WindowEvent we) { System.exit(0); }
                    });                    
                    
                    String [] items1 = { "land", "water" };                    
                    String [] items2 = {"indoor", "outdoor", "mixed"};  
                    
                    
                    // put the controls the content pane
                    Container c = f.getContentPane();
                    JPanel panel = new JPanel();
                    
                    panel.add(new JLabel("Please make a selection:"));
                    DefaultComboBoxModel model = new DefaultComboBoxModel();
                    final JList list = new JList(items1);
                    
                    JButton button = new JButton("Per favore");
                        button.addActionListener(new ActionListener( ) {
                          public void actionPerformed(ActionEvent ae) {
                            Object[] selection = list.getSelectedValues( );
                            System.out.println("-----");
                            for (int i = 0; i < selection.length; i++)
                              System.out.println(selection[i]);
                          }
                        });                    
                    
                    
                    model.addElement("env");
                    model.addElement("loc");
                    model.addElement("cost");
                    JComboBox comboBox = new JComboBox(model);
                    panel.add(comboBox);
                    
                    
                    c.add(panel, BorderLayout.NORTH);
                    c.add(new JScrollPane(list), BorderLayout.CENTER);
                    c.add(button, BorderLayout.SOUTH);

                    f.setVisible(true);                    
                    

                    /*int result = JOptionPane.showConfirmDialog(null, panel, "Flavor", JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE);*/
                    
                    
                    String svar;
                    svar = comboBox.getSelectedItem().toString();
                    
                    switch (svar) {
                        case "env":
                            list.setListData(items2);
                            break;
                    }
                    
            return desc1;
        }        





        public int countLines(JTextPane textArea) {
            String text = textArea.getText();
            String lines[];
            lines = text.split("\n");
            int count = lines.length;
            System.out.println ("COUNT LINES \n: "+count);
            return count;
            
        }    
        
        
        public String getLine(JTextPane textArea, int numLine) throws BadLocationException {        
            Document doc = textArea.getDocument();
            Element root = doc.getDefaultRootElement();
            Element element = root.getElement(numLine);
            int start = element.getStartOffset();
            int end = element.getEndOffset();
            System.out.println(doc.getText(start, end - start));        
            return doc.getText(start, end - start);
        }    
        
        
        
	//public void saveInput(String assigment) 
        public void saveInput(String input) throws IOException, Exception {
            

            File f= new File("/home/jorge/sw/code/sdata/04_input.txt");   //file to be delete  
            f.delete();            
            
            
            String fileName = "/home/jorge/sw/code/sdata/04_input.txt";
	    FileWriter fileWriter = new FileWriter(fileName);
            System.out.println ("Inside saveInput 1");
            try (PrintWriter printWriter = new PrintWriter(fileWriter)) {
                System.out.println ("Inside saveInput 2");
                //printWriter.print("Some String");
                printWriter.printf(input);
            }

            ExeProc("pro_brev");
            
            
            
	}        
        
    
    
}