/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package chatbot;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import static java.lang.System.console;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
public class MyDialog2 extends JDialog implements ActionListener {
   private String[] data;
   private JTextField descBox;
   private JComboBox<String> colorList;
   private JComboBox<String> colorList2;
   private JList listval;
   private JList listval2;
   private JButton btnOk;
   private JButton btnCancel;
   private JCheckBox checkbox;
   private JCheckBox checkbox2;
   /*
    String[] colorStrings = {"env","loc","cost","soc"};
   
    String [] items1 = { "land", "water" }; 
    String [] items2 = { "indoor", "outdoor", "mixed"};       
    String [] items3 = { "low", "med", "high"};        
    String [] items4 = { "single", "team", "mixed"};  
   */
    
    //String[] colorStrings = {"env","loc","soc","cost","danger","intens"};
    String[] colorStrings = {"environment","location","sociality","cost","dangerousness","intensity"};
   
    String [] items1 = { "land", "water" }; 
    String [] items2 = { "indoor", "outdoor", "mixed"};       
    String [] items3 = { "single", "team", "mixed"};    
    String [] items4 = { "low", "med", "high"};            
    String [] items5 = { "low", "med", "high"};            
    String [] items6 = { "low", "med", "high"};       
    
    
    
    
   
   
   public MyDialog2(Frame parent) {
       
      super(parent,"Please, choose your preferences :",true);
      Point loc = parent.getLocation();
      setLocation(loc.x+80,loc.y+80);
      
      data = new String[4]; // set to amount of data items
      JPanel panel = new JPanel();
      panel.setLayout(new GridBagLayout());
      GridBagConstraints gbc = new GridBagConstraints();
      gbc.insets = new Insets(15,15,15,15);
  
      
      
      
      JLabel colorLabel = new JLabel("Variable :");
      gbc.gridwidth = 1;
      gbc.gridx = 1;
      gbc.gridy = 0;
      panel.add(colorLabel,gbc);
      
      colorList = new JComboBox<String>(colorStrings);
      colorList.addActionListener(this);
      gbc.gridwidth = 1;
      gbc.gridx = 1;
      gbc.gridy = 1;
      panel.add(colorList,gbc);
      
      
      JLabel valueLabel = new JLabel("                  Value");
      gbc.gridwidth = 1;
      gbc.gridx = 3;
      gbc.gridy = 0;
      panel.add(valueLabel,gbc);   
      
      checkbox = new JCheckBox("Not"); 
      checkbox.setSelected(false);      
      gbc.gridwidth = 1;
      gbc.gridx = 3;
      gbc.gridy = 1;
      panel.add(checkbox,gbc);        
     
      listval = new JList(items1);
      gbc.gridx = 4;
      gbc.gridy = 1;
      panel.add(listval,gbc);      
      
      /*JLabel spacer = new JLabel(" ");
      gbc.gridx = 0;
      gbc.gridy = 2;
      panel.add(spacer,gbc);*/

      /**************************************/
      /*
      JLabel colorLabel2 = new JLabel("Variable :");
      gbc.gridwidth = 1;
      gbc.gridx = 0;
      gbc.gridy = 2;
      panel.add(colorLabel2,gbc);*/
      
      colorList2 = new JComboBox<String>(colorStrings);
      colorList2.addActionListener(this);
      gbc.gridwidth = 1;
      gbc.gridx = 1;
      gbc.gridy = 2;
      panel.add(colorList2,gbc);
      
      /*
      JLabel valueLabel2 = new JLabel("Value :");
      gbc.gridwidth = 1;
      gbc.gridx = 2;
      gbc.gridy = 2;
      panel.add(valueLabel2,gbc);   
      */
      checkbox2 = new JCheckBox("Not"); 
      checkbox2.setSelected(false);      
      gbc.gridwidth = 1;
      gbc.gridx = 3;
      gbc.gridy = 2;
      panel.add(checkbox2,gbc);        
     
      listval2 = new JList(items1);
      gbc.gridx = 4;
      gbc.gridy = 2;
      panel.add(listval2,gbc);      
      
      JLabel spacer = new JLabel(" ");
      gbc.gridx = 0;
      gbc.gridy = 3;
      panel.add(spacer,gbc);      
      
      
      /*************************************/
      
      btnOk = new JButton("Ok");
      btnOk.addActionListener(this);
      gbc.gridwidth = 1;
      gbc.gridx = 4;
      gbc.gridy = 4;
      panel.add(btnOk,gbc);
      
      btnCancel = new JButton("Cancel");
      btnCancel.addActionListener(this);
      gbc.gridx = 3;
      gbc.gridy = 4;
      panel.add(btnCancel,gbc);
      
      
      getContentPane().add(panel);
      pack();
   }
   
   public void actionPerformed(ActionEvent ae) {
      Object source = ae.getSource();
      String svar,svar2;

      if (source == colorList) {
          svar = (String)colorList.getSelectedItem();
          //System.out.println("Entro al "+svar);
          switch (svar) {
                        case "environment":
                            listval.setListData(items1);
                            //System.out.println("Entro al env");
                            break;
                        case "location":
                            listval.setListData(items2);
                            //System.out.println("Entro al loc");
                            break;          
                        case "sociality":
                            listval.setListData(items3);
                            //System.out.println("Entro al soc");
                            break;                             
                        case "cost":
                            listval.setListData(items4);
                            //System.out.println("Entro al cost");
                            break;                                                         
                        case "dangerousness":
                            listval.setListData(items5);
                            //System.out.println("Entro al cost");
                            break;                                                         
                        case "intensity":
                            listval.setListData(items6);
                            //System.out.println("Entro al cost");
                            break;      
                            
                            
                            
                            
                            
                            
          }
        }  
          
        if (source == colorList2) {
          svar2 = (String)colorList2.getSelectedItem();
          System.out.println("Entro al "+svar2);
          switch (svar2) {
                        case "environment":
                            listval2.setListData(items1);
                            //System.out.println("Entro al env");
                            break;
                        case "location":
                            listval2.setListData(items2);
                            //System.out.println("Entro al loc");
                            break;          
                        case "sociality":
                            listval2.setListData(items3);
                            //System.out.println("Entro al soc");
                            break;                             
                        case "cost":
                            listval2.setListData(items4);
                            //System.out.println("Entro al cost");
                            break;                                                         
                        case "dangerousness":
                            listval2.setListData(items5);
                            //System.out.println("Entro al cost");
                            break;                                                         
                        case "intensity":
                            listval2.setListData(items6);
                            //System.out.println("Entro al cost");
                            break;     
                            
                            
                            
                            
                            
          }
        } 
      
      
      if (source == btnOk) {
         /*data[0] = descBox.getText();*/
         /*data[0] = list.getSelectedValue();*/
         
         //data[1] = (String)colorList.getSelectedItem();
         data[1] = getVar((String)colorList.getSelectedItem());
         
         //data[3] = (String)colorList2.getSelectedItem();
         data[3] = getVar((String)colorList2.getSelectedItem());
         
        if (checkbox.isSelected()) {
            data[0] = "not "+(String)listval.getSelectedValue();
        } else {
             data[0] = (String)listval.getSelectedValue();
        }
        
        if (checkbox2.isSelected()) {
            data[2] = "not "+(String)listval2.getSelectedValue();
        } else {
             data[2] = (String)listval2.getSelectedValue();
        }        
        
        
         dispose();
      }
      else {
         data[0] = null;
         data[2] = null;
      }
      /*dispose();*/
   }
   public String[] run() {
      this.setVisible(true);
      return data;
   }
   

    private String getVar(String svarLong) {
            String svar = null;
            switch (svarLong) {
                        case "environment":
                            svar = "env";
                            break;
                        case "location":
                            svar = "loc";
                            break;          
                        case "sociality":
                            svar = "soc";
                            break;                             
                        case "cost":
                            svar = "cost";
                            break;                                                         
                        case "dangerousness":
                            svar = "danger";
                            break;                                                         
                        case "intensity":
                            svar = "intens";
                            break;                                                                                     
          }        
            return svar;
        
    }
   
}