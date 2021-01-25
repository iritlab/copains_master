/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package chatbot;

    import javax.swing.*;
    import java.awt.*;
    class TextPane
    {
    	public static void main(String[] args)
    	{
    		JFrame jf = new JFrame();
    		JPanel instructionPanel = new JPanel();
    		jf.setLayout(null);
    		
    			JTextPane instructionTextPanel = new javax.swing.JTextPane();
    			JScrollPane slider = new JScrollPane(instructionTextPanel);
                        
    			// instructionTextPanel.add(slider);
    			//slider.addAdjustmentListener(this);
                        slider.setBounds(6,7,175,179);
                        
    			instructionTextPanel.setFont(new java.awt.Font("MS Sans Serif", 1, 14));
    			instructionTextPanel.setText("instructions will go in here!");
    			instructionPanel.add(slider);
    		//	instructionTextPanel.disable();
    			instructionTextPanel.setBounds(6,7,175,179);
    			instructionTextPanel.setBorder(new javax.swing.border.TitledBorder(null, "Instructions", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Tahoma", 1, 16), new java.awt.Color(0, 102, 102)));
    			
    			instructionPanel.setBounds(0,0,190,260);
                        
    			jf.getContentPane().add(instructionPanel);
    		
    		jf.setBounds(100,100,400,300);
    		jf.setVisible(true);
    		
    	}
    }