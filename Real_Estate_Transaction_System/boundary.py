import tkinter as tk
from tkinter import *
from tkinter import ttk, messagebox, filedialog
from PIL import ImageTk, Image
import re
import hashlib
import shutil
import os
import controller
import images

class UserLoginPage:
    def __init__(self, root):
        self.root = root
        self.displayUserLoginPage()
        
    def displayUserLoginPage(self):
        
        # base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # background image
        bg_image = Image.open('images/background.png')
        photo = ImageTk.PhotoImage(bg_image)
        bg_panel = Label(self.home_frame, image=photo)
        bg_panel.image = photo
        bg_panel.pack(fill='both', expand='yes')

        heading_frame = Frame(self.home_frame, bg='white')
        heading_frame.place(relx=0.15, rely=0.1, relwidth=0.35, relheight=0.3)

        heading = Label(heading_frame, text="HOME SMITH", font=('Palatino', 35, "bold"), bg="white",fg='#555',bd=5,relief=FLAT)
        heading.pack(side='top', fill='both', expand='yes', padx=10, pady=10)

        side_image_frame = Frame(self.home_frame, bg='white')
        side_image_frame.place(relx=0.15, rely=0.4, relwidth=0.35, relheight=0.5)

        # Left Side Image
        side_image = Image.open('images/real.png')
        photo = ImageTk.PhotoImage(side_image)
        side_image_label = Label(side_image_frame, image=photo, bg='white')
        side_image_label.image = photo
        side_image_label.pack(anchor='center', fill='both', expand='yes', padx=10)

        sign_in_image_frame = Frame(self.home_frame, bg='white')
        sign_in_image_frame.place(relx=0.5, rely=0.1, relwidth=0.35, relheight=0.3)

        # Sign In Image 
        sign_in_image = Image.open('images/home.png')
        photo = ImageTk.PhotoImage(sign_in_image)
        sign_in_image_label = Label(sign_in_image_frame, image=photo, bg='white')
        sign_in_image_label.image = photo
        sign_in_image_label.pack(side='top', fill='both', expand='yes', padx=10, pady=10)

        # Welcome label 
        welcome_label = Label(sign_in_image_frame, text="Welcome to\nHomeSmith Real Estate System!!", bg="white", fg="#4f4e4d",font=("", 13, "bold"))
        welcome_label.pack(side='bottom', expand='yes', padx=10)

        # Login Frame 
        login_frame = Frame(self.home_frame, bg='white')
        login_frame.place(relx=0.5, rely=0.4, relwidth=0.35, relheight=0.5)
        login_frame.rowconfigure((0,1,2,3), weight=1, uniform='a')
        login_frame.columnconfigure((0), weight=1, uniform='a')

        # Sign In label 
        sign_in_label = Label(login_frame, text="Sign In", bg="white", fg="#4f4e4d",font=("", 14, "bold"))
        sign_in_label.grid(row=0, column=0, sticky='nwse')

        # username
        username_frame = Frame(login_frame, bg='white')
        username_frame.grid(row=1, column=0, sticky='nwse')

        username_label = Label(username_frame, text="User ID", bg="white", fg="#4f4e4d",font=("", 13, "bold"))
        username_label.place(relx=0.15, rely=0.1)

        self.user_id_entry = Entry(username_frame, highlightthickness=0, relief=FLAT, bg="white", fg="#6b6a69", font=("", 13, "bold"), insertbackground = '#6b6a69')
        self.user_id_entry.place(relx=0.22, rely=0.5, relwidth=0.58)

        username_line = Canvas(username_frame, width=300, height=2.0, bg="#bdb9b1", highlightthickness=0)
        username_line.place(relx=0.15, rely=0.8, relwidth=0.65)

        # Username icon
        username_icon = Image.open('images/username_icon.png')
        photo = ImageTk.PhotoImage(username_icon)
        username_icon_label = Label(username_frame, image=photo, bg='white')
        username_icon_label.image = photo
        username_icon_label.place(relx=0.15, rely=0.5)#x=550, y=332

        # password
        self.password_frame = Frame(login_frame, bg='white')
        self.password_frame.grid(row=2, column=0, sticky='nwse')

        password_label = Label(self.password_frame, text="Password", bg="white", fg="#4f4e4d",font=("", 13, "bold"))
        password_label.place(relx=0.15, rely=0.1)

        self.password_entry = Entry(self.password_frame, highlightthickness=0, relief=FLAT, bg="white", fg="#6b6a69", font=("", 13, "bold"), show="*", insertbackground = '#6b6a69')
        self.password_entry.place(relx=0.22, rely=0.5, relwidth=0.58)

        password_line = Canvas(self.password_frame, width=300, height=2.0, bg="#bdb9b1", highlightthickness=0)
        password_line.place(relx=0.15, rely=0.8, relwidth=0.65)

        # Password icon 
        password_icon = Image.open('images/password_icon.png')
        photo = ImageTk.PhotoImage(password_icon)
        password_icon_label = Label(self.password_frame, image=photo, bg='white')
        password_icon_label.image = photo
        password_icon_label.place(relx=0.15, rely=0.5)

        # show/hide password 
        self.show_image = ImageTk.PhotoImage(file='images/show.png')
        self.hide_image = ImageTk.PhotoImage(file='images/hide.png')

        self.show_button = Button(self.password_frame, image=self.show_image, command=self.show, relief=FLAT,activebackground="white",
                            borderwidth=0, background="white", cursor="hand2", )
        self.show_button.place(relx=0.75, rely=0.6, relwidth=0.05)

        # login button
        loginbth_frame = Frame(login_frame, bg='white')
        loginbth_frame.grid(row=3, column=0, sticky='nwse')

        loginbtn = Button(loginbth_frame, text="Login", font=("", 13), bg='lightgray', fg='black', border=1, 
                          highlightbackground='#57a1f8', cursor='hand2', command=self.loginAccount)  
        loginbtn.place(relx=0.5, rely=0.4, anchor='center', relheight=0.3, relwidth=0.2)
    
    def show(self):
        self.hide_button = Button(self.password_frame, image=self.hide_image, command=self.hide, relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
        self.hide_button.place(relx=0.75, rely=0.6, relwidth=0.05)
        self.password_entry.config(show='')

    def hide(self):
        self.show_button = Button(self.password_frame, image=self.show_image, command=self.show, relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
        self.show_button.place(relx=0.75, rely=0.6, relwidth=0.05)
        self.password_entry.config(show='*')

    def loginAccount(self):
        user_id = self.user_id_entry.get()
        password = self.password_entry.get()
        hashed_Password = self.hash_password(password)

        # Create an instance of UserLoginController
        login_controller = controller.UserLoginController()

        # Call the userLogin method to attempt login
        user = login_controller.loginAccount(user_id, hashed_Password)

        if user:
            if (user.suspend == 1):
                messagebox.showerror("Login Failed", "Your Account is suspended! Contact admin team for further process")
            elif user.profileName == "system admin":
                self.openAdminPage(user)
            elif user.profileName == "real estate agent":
                self.openAgentHomePage(user)
            elif user.profileName == "buyer":
                self.openBuyerPage(user)
            elif user.profileName == "seller":
                self.openSellerPage(user)
            else:
                self.openUnderDevelopmentPage(user)
        else:
            messagebox.showerror("Login Failed", "Invalid username or password! Please try again.")

    def hash_password(self, password):
        # Convert the password string to bytes
        password_bytes = password.encode('utf-8')

        # Create a SHA-256 hash object
        sha256_hash = hashlib.sha256()

        # Update the hash object with the password bytes
        sha256_hash.update(password_bytes)

        # Get the hexadecimal representation of the hash
        hashed_password = sha256_hash.hexdigest()

        return hashed_password
    
    def openAdminPage(self, user):
        self.home_frame.pack_forget()
        AdminManageUserAccountsPage(self.root, user)

    def openAgentHomePage(self, user):
        self.home_frame.pack_forget()
        AgentHomePage(self.root, user)

    def openBuyerPage(self, user):
        self.home_frame.pack_forget()
        BuyerHomePage(self.root, user)

    def openSellerPage(self, user):
        self.home_frame.pack_forget()
        SellerHomePage(self.root, user)

    def openUnderDevelopmentPage(self, user):
        self.home_frame.pack_forget()
        UnderDevelopmentPage(self.root, user)

class UserLogoutPage:
    def __init__(self, root, frame):
        self.root = root
        self.frame = frame

        self.root1 = Tk()
        self.root1.geometry("450x250")
        self.root1.title("Logout")
        self.displayUserLogoutPage()

    def displayUserLogoutPage(self):
        # Create a frame to hold the logout content
        self.logoutframe = Frame(self.root1, bg="lightgrey")
        self.logoutframe.pack(expand=True, fill=BOTH, padx=20, pady=20)

        # Create a boundary rectangle shape
        self.canvas = Canvas(self.logoutframe, bg="white", highlightthickness=0)
        self.canvas.pack(expand=True, fill=BOTH)

        # Draw the boundary rectangle
        self.canvas.create_rectangle(10, 10, 390, 190, outline="black", width=2)

        # Add the text
        self.canvas.create_text(200, 60, text="You are attempting to log out of Real Estate System", font=("", 12), justify=tk.CENTER)
        self.canvas.create_text(200, 90, text="Are you sure?", font=("", 12), justify=CENTER)

        # Create buttons
        self.cancel_button = Button(self.logoutframe, text="Cancel", command=self.cancel_logout)
        self.logout_button = Button(self.logoutframe, text="Logout", command=self.confirm_logout)

        # Add buttons inside the rectangle boundary
        self.canvas.create_window(140, 140, window=self.cancel_button)
        self.canvas.create_window(260, 140, window=self.logout_button)

    def cancel_logout(self):
        self.root1.destroy()

    def confirm_logout(self):
        self.root1.destroy()
        self.frame.pack_forget()
        UserLoginPage(self.root)

class AdminManageUserAccountsPage:
    def __init__(self, root, admin):
       self.root = root
       self.admin = admin
       self.page = 'ManageAccount'
       self.displayAdminManageUserAccountsPage()

    def displayAdminManageUserAccountsPage(self):
       
        # base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)

        # Label "Admin ID"
        admin_label = tk.Label(header_frame, text=self.admin.userName, bg='lightblue', fg='black', font=("", 18, "bold"))
        admin_label.pack(side='left', padx=20, pady=10)

        # Logout button
        logout_btn = Button(header_frame, text='Logout', bg='lightyellow', font=("", 13, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.logout )
        logout_btn.pack(side='right', padx=10, pady=10)

        # check_profile button
        check_profile_btn = Button(header_frame, text='  Profile  ', bg='lightyellow', font=("", 13, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.openCheckAccountPage)
        check_profile_btn.pack(side='right', padx=10, pady=10)
                                
        # ManageProfile button
        manage_profile_btn = Button(header_frame, text='Manage Profile', bg='lightyellow', font=("", 13, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.openManageProfilePage)
        manage_profile_btn.pack(side='right', padx=10, pady=10)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # menu frame
        menu_frame = Frame(self.home_frame, bg='white', bd=1)
        menu_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.075)

        # Labels 
        labels = ["All", "Admin", "Buyer", "Seller", "Agent"]
        for label_text in labels:
            label = tk.Label(menu_frame, text=label_text, bg='white', fg='black', font=("", 13, "bold"), justify = 'center')
            label.pack(side='left', padx=(20,20), pady=10)
            
            if (label_text != "Agent"):
                # line seprator
                line = tk.Frame(menu_frame, bg='black', width=2, height=40)
                line.pack(side='left', pady=10)

        # create label
        create_label = tk.Label(menu_frame, text="Create", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        create_label.pack(side='right', padx=(20,20), pady=10)
        create_label.bind("<Button-1>", self.openCreateAccountPage)

        # Search UserID Entry
        self.search_entry = tk.Entry(menu_frame, bg='white', fg='black', font=("", 13))
        self.search_entry.insert(0, "Search User Name")
        self.search_entry.bind("<FocusIn>", lambda e: self.search_entry.delete(0, "end"))
        self.search_entry.pack(side='right', pady=10, padx=10)
        self.search_entry.bind("<Return>", self.searchUserAccount)

        # separator between home and average frames
        separator2 = ttk.Separator(menu_frame, orient='horizontal')
        separator2.place(relx=0, rely=0.98, relwidth=1, relheight=0.02)

        # listing Main Frame
        self.content_frame = Frame(self.home_frame, bg='lightyellow')
        self.content_frame.place(relx=0, rely=0.175, relwidth=1, relheight=0.825)
        self.content_frame.rowconfigure((0,1,2,3,4,5,6,7,8,9,10,11,12), weight=1, uniform='a')
        self.content_frame.columnconfigure((0), weight=1, uniform='a')

        self.viewAllUserAccounts()

    def viewAllUserAccounts(self):
        viewUser = controller.AdminViewAllUserAccountsController()
        UserAccounts = viewUser.viewAllUserAccounts()
        self.showUserAccountPages(0, UserAccounts, UserAccounts[:12])

    def searchUserAccount(self, evemt):

        searchUser = self.search_entry.get()

        if (searchUser == ""):
            messagebox.showerror("Failed", "Please enter a valid UserId")
            self.viewAllUserAccounts()

        searchAccountController = controller.AdminSearchUserAccountController()
        searchAccount = searchAccountController.searchUserAccount(searchUser)

        self.showUserAccountPages(0, searchAccount, searchAccount[:12])

    def showUserAccountPages(self, page_index, all_accounts, show_accounts):

        for widget in self.content_frame.winfo_children():
                widget.destroy()

        current_page = page_index

        if len(all_accounts) == 0:
            page_number = 1
        elif len(all_accounts) % 12 == 0:
            page_number = int(len(all_accounts)/12)  
        else:
            page_number = int(len(all_accounts)/12) + 1


        btn_frame = Frame(self.content_frame, bg='lightyellow')
        #btn_frame.place(relx=0, rely=0.9, relwidth=1, relheight=0.1)
        btn_frame.grid(row=12, column=0, sticky='nwse')
        btn_frame.rowconfigure((0), weight=1, uniform='a')
        btn_frame.columnconfigure((0,1,2,3,4,5,6,7,8), weight=1, uniform='a')

        # number label
        number_label = tk.Label(btn_frame, text=f"{current_page+1}/{page_number}", bg='lightyellow', fg='black', font=("", 13))
        number_label.grid(row=0, column=4, sticky='nwse')

        # previous label
        previous_label = tk.Label(btn_frame, text="<<", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        previous_label.grid(row=0, column=4, sticky='w')
        previous_label.bind("<Button-1>", lambda event, page_index=current_page-1: self.showUserAccountPages(page_index, all_accounts, all_accounts[page_index*12:(page_index*12)+12]) if page_index >= 0 else None)

        # next label
        next_label = tk.Label(btn_frame, text=">>", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        next_label.grid(row=0, column=4, sticky='e')
        next_label.bind("<Button-1>", lambda event, page_index=current_page+1: self.showUserAccountPages(page_index, all_accounts, all_accounts[page_index*12:(page_index*12)+12]) if page_index < page_number else None)


        for i,account in enumerate(show_accounts):

            account_frame = Frame(self.content_frame, bg='lightyellow')
            account_frame.grid(row=i, column=0, sticky='nwse')

            # Labels
            number_label = tk.Label(account_frame, text =i+1 , bg='lightyellow', fg='black', font=("", 13))
            number_label.place(relx=0.02, rely=0.5, anchor=tk.W)

            # Labels
            id_label = tk.Label(account_frame, text = account.user_id , bg='lightyellow', fg='black', font=("", 13))
            id_label.place(relx=0.05, rely=0.5, anchor=tk.W)

            roles_label = tk.Label(account_frame, text=account.profileName, bg='lightyellow', fg='black', font=("", 13))
            roles_label.place(relx=0.25, rely=0.5, anchor=tk.W)

            if account.suspend == 0 :
                suspend_status = 'active'
            elif account.suspend == 1 :
                suspend_status = 'suspended'

            status_label = tk.Label(account_frame, text=suspend_status, bg='lightyellow', fg='black', font=("", 13, "bold"))
            status_label.place(relx=0.4, rely=0.5, anchor=tk.W)

            view_button = tk.Button(account_frame, text="View", bg='lightgrey', fg='black', font=("", 12),
                            command=lambda user_id=account.user_id: self.openViewAccountInfoPage(user_id))
            view_button.place(relx=0.8, rely=0.5, anchor=tk.W, width=90)

            if account.suspend == 0:
                suspend_button = tk.Button(account_frame, text="Suspend", bg='lightgrey', fg='black', font=("", 12),
                                    command=lambda user_id=account.user_id: self.openSuspendAccountPage(user_id))
                suspend_button.place(relx=0.9, rely=0.5, anchor=tk.W, width=90)
            elif account.suspend == 1:
                unsuspend_button = tk.Button(account_frame, text="Unsuspend", bg='lightgrey', fg='black', font=("", 12),
                                    command=lambda user=account: self.openUnsuspendAccountPage(user))
                unsuspend_button.place(relx=0.9, rely=0.5, anchor=tk.W, width=90)

            # separator between home and average frames
            separator = ttk.Separator(account_frame, orient='horizontal')
            separator.place(relx=0, rely=0.98, relwidth=1, relheight=0.02)

    def openManageProfilePage(self):
        self.home_frame.pack_forget()
        AdminManageUserProfilesPage(self.root, self.admin)

    def logout(self):
        UserLogoutPage(self.root, self.home_frame)

    def openSuspendAccountPage(self, user_id):
        AdminSuspendUserAccountPage(self.root, self.home_frame, self.admin, user_id)

    def openUnsuspendAccountPage(self, user):
        AdminUnsuspendUserAccountPage(self.root, self.home_frame, self.admin, user)

    def openCreateAccountPage(self, event):
        self.home_frame.pack_forget()
        AdminCreateUserAccountPage(self.root, self.admin)

    def openViewAccountInfoPage(self, user_id):
        self.home_frame.pack_forget()
        AdminViewAccountInfoPage(self.root, self.admin, user_id)

    def openCheckAccountPage(self):
        self.home_frame.pack_forget()
        UserViewMyAccountPage(self.root, self.admin.user_id, self.page)

class AdminViewAccountInfoPage:
    def __init__(self, root, admin, user_id):
        self.root = root
        self.admin = admin
        self.user_id = user_id

        self.viewAccountInfo()

    def viewAccountInfo (self):

        self.viewAccountController = controller.AdminViewAccountInfoController()
        self.user = self.viewAccountController.viewAccountInfo(self.user_id)
 
        self.displayAdminViewAccountInfoPage()

    def displayAdminViewAccountInfoPage(self):
        #base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)
        
        # Title Label
        label_title = tk.Label(header_frame, text="View Account", bg="lightblue",  fg='black', font=("", 22, "bold"))
        label_title.pack(expand=True, anchor=tk.CENTER)
    
        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)
        
        # 2nd Frame
        content_frame = tk.Frame(self.home_frame, bg="white")
        content_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.9)# Allow content frame to expand
        
        # userId Label
        label_userid = tk.Label(content_frame, text="User ID", bg="white", padx=10, pady=5, font=("", 15))
        label_userid.place(relx=0.3, rely=0.1, anchor=tk.W)
        
        # userId 
        userId = tk.Label(content_frame, text=self.user.user_id, bg="white", font=("", 15))
        userId.place(relx=0.55, rely=0.1, anchor=tk.W)

        # Name Label
        label_name = tk.Label(content_frame, text="Name", bg="white", padx=10, pady=5, font=("", 15))
        label_name.place(relx=0.3, rely=0.2, anchor=tk.W)

        # Name
        name = tk.Label(content_frame, text=self.user.userName, bg="white", font=("", 15))
        name.place(relx=0.55, rely=0.2, anchor=tk.W)
        
        # Phone Label
        label_phone = tk.Label(content_frame, text="Phone Number", bg="white", padx=10, pady=5, font=("", 15))
        label_phone.place(relx=0.3, rely=0.3, anchor=tk.W)
        
        # Phone
        phone = tk.Label(content_frame, text=self.user.phoneNo, bg="white", font=("", 15))
        phone.place(relx=0.55, rely=0.3, anchor=tk.W)

        # Email Label
        label_email = tk.Label(content_frame, text="Email", bg="white", padx=10, pady=5, font=("", 15))
        label_email.place(relx=0.3, rely=0.4, anchor=tk.W)

        # email
        email = tk.Label(content_frame, text=self.user.mail, bg="white", font=("", 15))
        email.place(relx=0.55, rely=0.4, anchor=tk.W)

        # status Label
        label_status = tk.Label(content_frame, text="Status", bg="white", padx=10, pady=5, font=("", 15))
        label_status.place(relx=0.3, rely=0.5, anchor=tk.W)
        
        if self.user.suspend == 1:
            active_status = 'Suspended'
        elif self.user.suspend == 0:
            active_status = 'Active'

        # status
        status = tk.Label(content_frame, text=active_status, bg="white", font=("", 15))
        status.place(relx=0.55, rely=0.5, anchor=tk.W)

        # Password Label
        label_password = tk.Label(content_frame, text="Password", bg="white", padx=10, pady=5, font=("", 15))
        label_password.place(relx=0.3, rely=0.6, anchor=tk.W)
        
        # password
        password = tk.Label(content_frame, text= "**********", bg="white", font=("", 15))
        password.place(relx=0.55, rely=0.6, anchor=tk.W)
        
        # role Label
        label_role = tk.Label(content_frame, text="Role", bg="white", padx=10, pady=5, font=("", 15))
        label_role.place(relx=0.3, rely=0.7, anchor=tk.W)

        # role 
        role = tk.Label(content_frame, text=self.user.profileName, bg="white", font=("", 15))
        role.place(relx=0.55, rely=0.7, anchor=tk.W)
        
        #Cancel Button
        cancel_button = tk.Button(content_frame,  text="Cancel",  bg="lightgray", padx=10, pady=5, font=("", 12), command=self.openAdminHomePage)
        cancel_button.place(relx=0.47, rely=0.85, anchor=tk.E, width=100)
        
        edit_button = tk.Button(content_frame,  text="Edit",  bg="lightgray", padx=10, pady=5, font=("", 12), command=self.openUpdateAccountPage)
        edit_button.place(relx=0.53, rely=0.85, anchor=tk.W, width=100)

    def openAdminHomePage(self):
        self.home_frame.pack_forget()
        AdminManageUserAccountsPage(self.root, self.admin)  

    def openUpdateAccountPage(self):
        if self.user.suspend == 1 :
            messagebox.showerror("Failed","The user is suspended! Please Unsuspend First to update the information.")
        else:
            self.home_frame.pack_forget()
            AdminUpdateUserAccountPage(self.root, self.admin, self.user)  

class AdminCreateUserAccountPage:
    def __init__(self, root, admin):
        self.root = root
        self.admin = admin
        self.displayAdminCreateUserAccountPage()

    def displayAdminCreateUserAccountPage(self):
        
        # base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)
        
        # Title Label
        label_title = tk.Label(header_frame, text="Create Account", bg="lightblue",  fg='black', font=("", 22, "bold"))
        label_title.pack(expand=True, anchor=tk.CENTER)
    
        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)
        
        # 2nd Frame
        self.content_frame = tk.Frame(self.home_frame, bg="white")
        self.content_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.9)# Allow content frame to expand
        
        # userId Label
        label_userid = tk.Label(self.content_frame, text="User ID", bg="white", padx=10, pady=5, font=("", 15))
        label_userid.place(relx=0.3, rely=0.1, anchor=tk.W)
        
        # Entry widget for userId
        self.entry_userId = tk.Entry(self.content_frame, font=("", 15), bd=1, relief=tk.SOLID)
        self.entry_userId.place(relx=0.55, rely=0.1, anchor=tk.W, relwidth=0.2)

        # Name Label
        label_name = tk.Label(self.content_frame, text="Name", bg="white", padx=10, pady=5, font=("", 15))
        label_name.place(relx=0.3, rely=0.2, anchor=tk.W)
        
        # Entry widget for name
        self.entry_name = tk.Entry(self.content_frame, font=("", 15), bd=1, relief=tk.SOLID)
        self.entry_name.place(relx=0.55, rely=0.2, anchor=tk.W,  relwidth=0.2)
        
        # Phone Label
        label_phone = tk.Label(self.content_frame, text="Phone Number", bg="white", padx=10, pady=5, font=("", 15))
        label_phone.place(relx=0.3, rely=0.3, anchor=tk.W)
        
        # Entry widget for phone
        self.entry_phone = tk.Entry(self.content_frame, font=("", 15), bd=1, relief=tk.SOLID)
        self.entry_phone.place(relx=0.55, rely=0.3, anchor=tk.W, relwidth=0.2)
        
        # Email Label
        label_email = tk.Label(self.content_frame, text="Email", bg="white", padx=10, pady=5, font=("", 15))
        label_email.place(relx=0.3, rely=0.4, anchor=tk.W)
        
        # Entry widget for email
        self.entry_email = tk.Entry(self.content_frame, font=("", 15), bd=1, relief=tk.SOLID)
        self.entry_email.place(relx=0.55, rely=0.4, anchor=tk.W, relwidth=0.2)
        
        # Password Label
        label_password = tk.Label(self.content_frame, text="Password", bg="white", padx=10, pady=5, font=("", 15))
        label_password.place(relx=0.3, rely=0.5, anchor=tk.W)
        
        # Entry widget for password
        self.entry_password = tk.Entry(self.content_frame, font=("", 15), bd=1, relief=tk.SOLID, show='*')
        self.entry_password.place(relx=0.55, rely=0.5, anchor=tk.W, relwidth=0.2)

                # show/hide password 
        self.show_image = ImageTk.PhotoImage(file='images/show.png')
        self.hide_image = ImageTk.PhotoImage(file='images/hide.png')

        self.show_button1 = tk.Button(self.content_frame, image=self.show_image, command=lambda: self.show(1), relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
        self.show_button1.place(relx=0.745, rely=0.5, anchor=tk.E)
        
        # Re-type Password Label
        label_retypepassword = tk.Label(self.content_frame, text="Re-type Password", bg="white", padx=10, pady=5, font=("", 15))
        label_retypepassword.place(relx=0.3, rely=0.6, anchor=tk.W)
        
        # Entry widget for Re-type password
        self.reentry_password = tk.Entry(self.content_frame, font=("", 15), bd=1, relief=tk.SOLID, show='*')
        self.reentry_password.place(relx=0.55, rely=0.6, anchor=tk.W, relwidth=0.2)

        self.show_button2 = tk.Button(self.content_frame, image=self.show_image, command=lambda: self.show(2), relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
        self.show_button2.place(relx=0.745, rely=0.6, anchor=tk.E)
        
        # Role Label
        label_role = tk.Label(self.content_frame, text="Role", bg="white", padx=10, pady=5, font=("", 15))
        label_role.place(relx=0.3, rely=0.7, anchor=tk.W)

        # Dropdown List
        fetchProfiles = controller.SystemFetchUserProfilesController()
        roles = fetchProfiles.fetchUserProfiles()
        roles1 = [item[0] for item in roles]
        self.selected_role = tk.StringVar()
        self.selected_role.set("Role")
        self.dropdown_role = tk.OptionMenu(self.content_frame, self.selected_role, *roles1)
        self.dropdown_role.config(font=("", 15), justify='left')
        self.dropdown_role.place(relx=0.55, rely=0.7, anchor=tk.W, relwidth=0.2)
        
        #Cancel Button
        cancel_button = tk.Button(self.content_frame,  text="Cancel",  bg="lightgray", padx=10, pady=5, font=("", 12), command=self.openAdminHomePage)
        cancel_button.place(relx=0.47, rely=0.85, anchor=tk.E, width=100)
        
        #Register Button
        create_button = tk.Button(self.content_frame,  text="Create",  bg="lightgray", padx=10, pady=5, font=("", 12), command = self.createUserAccount)
        create_button.place(relx=0.53, rely=0.85, anchor=tk.W, width=100)
        
    def show(self, num):
        if num == 1:
            self.hide_button1 = Button(self.content_frame, image=self.hide_image, command=lambda: self.hide(1), relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
            self.hide_button1.place(relx=0.745, rely=0.5, anchor=tk.E)
            self.entry_password.config(show='')
        elif num == 2:
            self.hide_button2 = Button(self.content_frame, image=self.hide_image, command=lambda: self.hide(2), relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
            self.hide_button2.place(relx=0.745, rely=0.6, anchor=tk.E)
            self.reentry_password.config(show='')

    def hide(self,num):
        if num == 1:
            self.show_button1 = Button(self.content_frame, image=self.show_image, command=lambda: self.show(1), relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
            self.show_button1.place(relx=0.745, rely=0.5, anchor=tk.E)
            self.entry_password.config(show='*')
        if num == 2:
            self.show_button2 = Button(self.content_frame, image=self.show_image, command=lambda: self.show(2), relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
            self.show_button2.place(relx=0.745, rely=0.6, anchor=tk.E)
            self.reentry_password.config(show='*')

    def emptyVal(self, input):
        self.input = input
        if input == "":
            return True
        else:
            return False
        
    def createUserAccount(self):
        #create instance of account 
        self.CreateUserAccountController = controller.AdminCreateUserAccountController()

        #get all values
        user_id = self.entry_userId.get()
        password = self.entry_password.get()
        confirmPassword = self.reentry_password.get()
        name = self.entry_name.get()
        email = self.entry_email.get()
        phoneNum = self.entry_phone.get()
        selectedProfile = self.selected_role.get()
        suspend = 0
        passwordPattern = r"^(?=.*[a-zA-Z0-9])(?=.*[!@#$%^&*])(?=.*[a-z])(?=.*[A-Z]).{8,}$"
        emailPattern = r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"
        phonePattern = r"^[89]\d{7}$"
        while (True):
            if (self.emptyVal(user_id) or 
                self.emptyVal(password) or 
                self.emptyVal(confirmPassword) or 
                self.emptyVal(name) or 
                self.emptyVal(email) or 
                self.emptyVal(phoneNum)):
                messagebox.showerror("Failed", "Please check that you have enter all details.")
                break
            elif len(user_id) <= 8: 
                messagebox.showerror("Failed","UserId must be more than 8 characters")
                break
            elif re.match(phonePattern,phoneNum) is None:
                messagebox.showerror("Failed","Please enter a valid phone number.")
                break
            elif re.match(emailPattern,email) is None:
                messagebox.showerror("Failed","Please enter a valid email.")
                break
            elif re.match(passwordPattern,password) is None:
                messagebox.showerror("Failed","Password must be more than 8 characters and it must contain at least a lowercase, a uppercase, a special character and a number.")
                break
            elif password != confirmPassword:
                messagebox.showerror("Failed","Confirm password must be the same as password.")
                break
            elif selectedProfile == "Role":
                messagebox.showerror("Failed","Please select a role.")
                break
            else:
                hashed_Password = self.hash_password(password)
                userAcc = self.CreateUserAccountController.createUserAccount(user_id,name,selectedProfile,hashed_Password,email,phoneNum,suspend)

                if (userAcc):
                    messagebox.showinfo("Successful!", "Account Created Successfully!")
                else:
                    messagebox.showerror("Failed", "User Account already exist!")
                break

    def hash_password(self, password):
        # Convert the password string to bytes
        password_bytes = password.encode('utf-8')

        # Create a SHA-256 hash object
        sha256_hash = hashlib.sha256()

        # Update the hash object with the password bytes
        sha256_hash.update(password_bytes)

        # Get the hexadecimal representation of the hash
        hashed_password = sha256_hash.hexdigest()

        return hashed_password
    
    def openAdminHomePage(self):
        self.home_frame.pack_forget()
        AdminManageUserAccountsPage(self.root, self.admin)

class AdminUpdateUserAccountPage:
    def __init__(self, root, admin, user):
        self.root = root
        self.admin = admin
        self.user = user
        self.displayAdminUpdateUserAccountPage()

    def displayAdminUpdateUserAccountPage(self):

        #base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)
        
        # Title Label
        label_title = tk.Label(header_frame, text="Update Account", bg="lightblue",  fg='black', font=("", 22, "bold"))
        label_title.pack(expand=True, anchor=tk.CENTER)
    
        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)
        
        # 2nd Frame
        self.content_frame = tk.Frame(self.home_frame, bg="white")
        self.content_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.9)# Allow content frame to expand
        
        # userId Label
        label_userid = tk.Label(self.content_frame, text="User ID", bg="white", padx=10, pady=5, font=("", 15))
        label_userid.place(relx=0.3, rely=0.1, anchor=tk.W)
        
        # userId 
        self.userId = tk.Label(self.content_frame, text=self.user.user_id, bg="white", font=("", 15))
        self.userId.place(relx=0.55, rely=0.1, anchor=tk.W)

        # Name Label
        label_name = tk.Label(self.content_frame, text="Name", bg="white", padx=10, pady=5, font=("", 15))
        label_name.place(relx=0.3, rely=0.2, anchor=tk.W)

        # Entry widget for name
        self.entry_name = tk.Entry(self.content_frame, font=("", 15), bd=1, relief=tk.SOLID)
        self.entry_name.insert(0, self.user.userName)
        self.entry_name.place(relx=0.55, rely=0.2 , anchor=tk.W, relwidth=0.2)
        
        # Phone Label
        label_phone = tk.Label(self.content_frame, text="Phone Number", bg="white", padx=10, pady=5, font=("", 15))
        label_phone.place(relx=0.3, rely=0.3, anchor=tk.W)
        
        # Entry widget for phone
        self.entry_phone = tk.Entry(self.content_frame, font=("", 15), bd=1, relief=tk.SOLID)
        self.entry_phone.insert(0, self.user.phoneNo)
        self.entry_phone.place(relx=0.55, rely=0.3, anchor=tk.W, relwidth=0.2)

        # Email Label
        label_email = tk.Label(self.content_frame, text="Email", bg="white", padx=10, pady=5, font=("", 15))
        label_email.place(relx=0.3, rely=0.4, anchor=tk.W)

        # Entry widget for email
        self.entry_email = tk.Entry(self.content_frame, font=("", 15), bd=1, relief=tk.SOLID)
        self.entry_email.insert(0, self.user.mail)
        self.entry_email.place(relx=0.55, rely=0.4, anchor=tk.W, relwidth=0.2)

        # Password Label
        label_password = tk.Label(self.content_frame, text="Password", bg="white", padx=10, pady=5, font=("", 15))
        label_password.place(relx=0.3, rely=0.5, anchor=tk.W)
        
        # Entry widget for password
        self.entry_password = tk.Entry(self.content_frame, font=("", 15), bd=1, relief=tk.SOLID, show='*')
        self.entry_password.place(relx=0.55, rely=0.5, anchor=tk.W, relwidth=0.2)

        # show/hide password 
        self.show_image = ImageTk.PhotoImage(file='images/show.png')
        self.hide_image = ImageTk.PhotoImage(file='images/hide.png')

        self.show_button1 = tk.Button(self.content_frame, image=self.show_image, command=lambda: self.show(1), relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
        self.show_button1.place(relx=0.745, rely=0.5, anchor=tk.E)

        # Re-type Password Label
        label_retype_password = tk.Label(self.content_frame, text="Re-type Password", bg="white", padx=10, pady=5, font=("", 15))
        label_retype_password.place(relx=0.3, rely=0.6, anchor=tk.W)
        
        # Entry widget for Re-type password
        self.reentry_password = tk.Entry(self.content_frame, font=("", 15), bd=1, relief=tk.SOLID, show='*')
        self.reentry_password.place(relx=0.55, rely=0.6, anchor=tk.W, relwidth=0.2)
        
        self.show_button2 = tk.Button(self.content_frame, image=self.show_image, command=lambda: self.show(2), relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
        self.show_button2.place(relx=0.745, rely=0.6, anchor=tk.E)

        # role Label
        label_role = tk.Label(self.content_frame, text="Role", bg="white", padx=10, pady=5, font=("", 15))
        label_role.place(relx=0.3, rely=0.7, anchor=tk.W)
        
        # role 
        role = tk.Label(self.content_frame, text=self.user.profileName, bg="white", font=("", 15))
        role.place(relx=0.55, rely=0.7, anchor=tk.W)
        
        #Cancel Button
        cancel_button = tk.Button(self.content_frame,  text="Cancel",  bg="lightgray", padx=10, pady=5, font=("", 12), command=self.openAdminViewAccountPage)
        cancel_button.place(relx=0.47, rely=0.85, anchor=tk.E, width=100)
        
        update_button = tk.Button(self.content_frame,  text="Update",  bg="lightgray", padx=10, pady=5, font=("", 12), command=self.updateUserAccount)
        update_button.place(relx=0.53, rely=0.85, anchor=tk.W, width=100)

    def emptyVal(input):
        if input == "":
            return True
        else:
            return False
        
    def updateUserAccount(self):
        name = self.entry_name.get()
        password = self.entry_password.get()
        confirmPassword = self.reentry_password.get()
        email = self.entry_email.get()
        phone = self.entry_phone.get()
        
        passwordPattern = r"^(?=.*[a-zA-Z0-9])(?=.*[!@#$%^&*])(?=.*[a-z])(?=.*[A-Z]).{8,}$"
        emailPattern = r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"
        phonePattern = r"^[89]\d{7}$"

        user_id = self.user.user_id
        asd = 0 #this variable is for the the elif statement where the admin doesnt want to touch the password

        while (True):
            if ((name == self.user.userName) and (email == self.user.mail) and (int(phone) == self.user.phoneNo) and (password == "") and (confirmPassword == "")):
                messagebox.showerror("Failed","Edit the data in order to update")
                break

            elif ((password == "") and (confirmPassword == "")):
                password = self.user.pwd
                confirmPassword = self.user.pwd
                asd = 1
                print(password)
                print(confirmPassword)

            if name == "":
                messagebox.showerror("Failed","Please enter a name.")
                break
            elif re.match(phonePattern,phone) is None:
                messagebox.showerror("Failed","Please enter a valid phone number.")
                break
            elif re.match(emailPattern,email) is None:
                messagebox.showerror("Failed","Please enter a valid email.")
                break
            elif re.match(passwordPattern,password) is None and asd == 0:
                messagebox.showerror("Failed","Password must be more than 8 characters and it must contain at least a lowercase, a uppercase, a special character and a number.")
                break
            elif password != confirmPassword:
                messagebox.showerror("Failed","Confirm password must be the same as password.")
                break
            else:
                if asd != 1:
                    password = self.hash_password(password)
                updateAccountController = controller.AdminUpdateUserAccountController()
                result = updateAccountController.updateUserAccount(user_id, name, password, email, phone)
                if (result):
                    messagebox.showinfo("Successful!", "User account is updated successfully!")
                else:
                    messagebox.showerror("Failed!", "Updating user account is failed!")
                break

    def hash_password(self, password):
        # Convert the password string to bytes
        password_bytes = password.encode('utf-8')

        # Create a SHA-256 hash object
        sha256_hash = hashlib.sha256()

        # Update the hash object with the password bytes
        sha256_hash.update(password_bytes)

        # Get the hexadecimal representation of the hash
        hashed_password = sha256_hash.hexdigest()

        return hashed_password
    
    def show(self, num):
        if num == 1:
            self.hide_button1 = Button(self.content_frame, image=self.hide_image, command=lambda: self.hide(1), relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
            self.hide_button1.place(relx=0.745, rely=0.5, anchor=tk.E)
            self.entry_password.config(show='')
        elif num == 2:
            self.hide_button2 = Button(self.content_frame, image=self.hide_image, command=lambda: self.hide(2), relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
            self.hide_button2.place(relx=0.745, rely=0.6, anchor=tk.E)
            self.reentry_password.config(show='')

    def hide(self,num):
        if num == 1:
            self.show_button1 = Button(self.content_frame, image=self.show_image, command=lambda: self.show(1), relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
            self.show_button1.place(relx=0.745, rely=0.5, anchor=tk.E)
            self.entry_password.config(show='*')
        if num == 2:
            self.show_button2 = Button(self.content_frame, image=self.show_image, command=lambda: self.show(2), relief=FLAT,
                                  activebackground="white", borderwidth=0, background="white", cursor="hand2")
            self.show_button2.place(relx=0.745, rely=0.6, anchor=tk.E)
            self.reentry_password.config(show='*')

    def openAdminViewAccountPage(self):
        self.home_frame.pack_forget()
        AdminViewAccountInfoPage(self.root, self.admin, self.user.user_id)    

class AdminSuspendUserAccountPage:
    def __init__(self, root, passedFrame, admin, user_id):
        self.admin = admin
        self.root = root
        self.passedFrame = passedFrame
        self.user_id = user_id

        self.root1 = Tk()
        self.root1.geometry("450x250")
        self.root1.title("Suspend Account")
        self.displayAdminSuspendUserAccountPage()

    def displayAdminSuspendUserAccountPage(self):
        
        # Create a frame to hold the suspend content
        suspend_frame = Frame(self.root1, bg="lightgrey")
        suspend_frame.pack(expand=True, fill=BOTH, padx=20, pady=20)

        # Create a boundary rectangle shape
        canvas = Canvas(suspend_frame, bg="white", highlightthickness=0)
        canvas.pack(expand=True, fill=tk.BOTH)

        # Draw the boundary rectangle
        canvas.create_rectangle(10, 10, 390, 190, outline="black", width=2)

        # Add the text
        canvas.create_text(200, 60, text=f"You are about to suspend {self.user_id}", font=("", 12), justify=CENTER)
        canvas.create_text(200, 90, text="Are you sure?", font=("", 12), justify=CENTER)

        # Create buttons
        cancel_button = Button(suspend_frame, text="Cancel", command=self.cancel_suspend)
        suspend_button = Button(suspend_frame, text="Suspend", command=self.confirm_suspend)

        # Add buttons inside the rectangle boundary
        canvas.create_window(140, 140, window=cancel_button)
        canvas.create_window(260, 140, window=suspend_button)

    def cancel_suspend(self):
        # Close the suspend page without logging out
        self.root1.destroy()

    def confirm_suspend(self):
        # Create an instance of SuspendUserController
        suspend_controller = controller.AdminSuspendUserAccountController()
        print(self.user_id)
        # Call the suspend_user method to suspend the user by userID
        success = suspend_controller.suspendUserAccount(self.user_id)
        
        # Check if suspension was successful
        if success:
            messagebox.showinfo("Suspend Success", f"User with userID '{self.user_id}' suspended successfully.")

        # Close the application or perform other actions here
        self.root1.destroy()
        self.passedFrame.pack_forget()
        AdminManageUserAccountsPage(self.root, self.admin)

class AdminUnsuspendUserAccountPage:
    def __init__(self, root, passedFrame, admin, user):
        self.admin = admin
        self.root = root
        self.passedFrame = passedFrame
        self.user = user

        self.root1 = Tk()
        self.root1.geometry("450x250")
        self.root1.title("Unsuspend Account")
        self.displayAdminUnsuspendUserAccountPage()

    def displayAdminUnsuspendUserAccountPage(self):
        
        # Create a frame to hold the suspend content
        unsuspend_frame = Frame(self.root1, bg="lightgrey")
        unsuspend_frame.pack(expand=True, fill=BOTH, padx=20, pady=20)

        # Create a boundary rectangle shape
        canvas = Canvas(unsuspend_frame, bg="white", highlightthickness=0)
        canvas.pack(expand=True, fill=tk.BOTH)

        # Draw the boundary rectangle
        canvas.create_rectangle(10, 10, 390, 190, outline="black", width=2)

        # Add the text
        canvas.create_text(200, 60, text=f"You are about to unsuspend {self.user.user_id}", font=("", 12), justify=CENTER)
        canvas.create_text(200, 90, text="Are you sure?", font=("", 12), justify=CENTER)

        # Create buttons
        cancel_button = Button(unsuspend_frame, text="Cancel", command=self.cancel_unsuspend)
        unsuspend_button = Button(unsuspend_frame, text="Unuspend", command=self.confirm_unsuspend)

        # Add buttons inside the rectangle boundary
        canvas.create_window(140, 140, window=cancel_button)
        canvas.create_window(260, 140, window=unsuspend_button)

    def cancel_unsuspend(self):
        # Close the suspend page without logging out
        self.root1.destroy()

    def confirm_unsuspend(self):
        # Create an instance of SuspendUserController
        unsuspend_controller = controller.AdminUnsuspendUserAccountController()
        # Call the suspend_user method to suspend the user by userID
        success = unsuspend_controller.unsuspendUserAccount(self.user.user_id, self.user.profileName)
        
        # Check if suspension was successful
        if success:
            messagebox.showinfo("Unsuspend Success", f"User with userID '{self.user.user_id}' unsuspended successfully.")
        else:
            messagebox.showerror("Unsuspend Failed",f"'{self.user.profileName}' profile is susppended.")

        # Close the application or perform other actions here
        self.root1.destroy()
        self.passedFrame.pack_forget()
        AdminManageUserAccountsPage(self.root, self.admin)

class AdminManageUserProfilesPage:
   
    def __init__(self, root, admin):
      self.root = root
      self.admin = admin
      self.page = 'ManageProfile'

      self.displayAdminManageUserProfilesPage()
   
    def displayAdminManageUserProfilesPage(self):
       
       # base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)

        # Label "Admin ID"
        admin_label = tk.Label(header_frame, text=self.admin.userName, bg='lightblue', fg='black', font=("", 18, "bold"))
        admin_label.pack(side='left', padx=20, pady=10)

        # Logout button
        logout_btn = Button(header_frame, text='Logout', bg='lightyellow', font=("", 13, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.logout )
        logout_btn.pack(side='right', padx=10, pady=10)

        # check_profile button
        check_profile_btn = Button(header_frame, text='  Profile  ', bg='lightyellow', font=("", 13, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.openCheckAccountPage)
        check_profile_btn.pack(side='right', padx=10, pady=10)
                                
        # ManageProfile button
        manage_profile_btn = Button(header_frame, text='Manage Account', bg='lightyellow', font=("", 13, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.openManageAccountPage)
        manage_profile_btn.pack(side='right', padx=10, pady=10)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # menu frame
        menu_frame = Frame(self.home_frame, bg='white', bd=1)
        menu_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.075)

        # create label
        create_label = tk.Label(menu_frame, text="Create", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        create_label.pack(side='right', padx=(20,20), pady=10)
        create_label.bind("<Button-1>", self.openCreateProfilePage)

        # Search UserID Entry
        self.search_entry = tk.Entry(menu_frame, bg='white', fg='black', font=("", 13))
        self.search_entry.insert(0, "Search User Name")
        self.search_entry.bind("<FocusIn>", lambda e: self.search_entry.delete(0, "end"))
        self.search_entry.pack(side='right', pady=10, padx=10)
        self.search_entry.bind("<Return>", self.searchUserProfile)

        # separator between home and average frames
        separator2 = ttk.Separator(menu_frame, orient='horizontal')
        separator2.place(relx=0, rely=0.98, relwidth=1, relheight=0.02)

        # listing Main Frame
        self.content_frame = Frame(self.home_frame, bg='lightyellow')
        self.content_frame.place(relx=0, rely=0.175, relwidth=1, relheight=0.825)
        self.content_frame.rowconfigure((0,1,2,3,4,5,6,7,8,9,10,11,12), weight=1, uniform='a')
        self.content_frame.columnconfigure((0), weight=1, uniform='a')
       
        self.viewAllUserProfiles()

    def viewAllUserProfiles(self):
        viewAllProfilesController = controller.AdminViewAllUserProfilesController()
        userProfiles = viewAllProfilesController.viewAllUserProfiles()
        self.showUserProfilePages(0, userProfiles, userProfiles[:12])

    def searchUserProfile(self, event):

        searchProfile = self.search_entry.get()

        if (searchProfile == ""):
            messagebox.showerror("Failed", "Please enter a valid profile")
            self.viewAllUserProfiles()

        searchProfileController = controller.AdminSearchUserProfileController()
        searchProfiles = searchProfileController.searchUserProfile(searchProfile)

        self.showUserProfilePages(0, searchProfiles, searchProfiles[:12])
    
    def showUserProfilePages(self, page_index, all_profiles, show_profiles):

        for widget in self.content_frame.winfo_children():
                widget.destroy()

        current_page = page_index

        if len(all_profiles) == 0:
            page_number = 1
        elif len(all_profiles) % 12 == 0:
            page_number = int(len(all_profiles)/12)  
        else:
            page_number = int(len(all_profiles)/12) + 1


        btn_frame = Frame(self.content_frame, bg='lightyellow')
        btn_frame.grid(row=12, column=0, sticky='nwse')
        btn_frame.rowconfigure((0), weight=1, uniform='a')
        btn_frame.columnconfigure((0,1,2,3,4,5,6,7,8), weight=1, uniform='a')

        # number label
        number_label = tk.Label(btn_frame, text=f"{current_page+1}/{page_number}", bg='lightyellow', fg='black', font=("", 13))
        number_label.grid(row=0, column=4, sticky='nwse')

        # previous label
        previous_label = tk.Label(btn_frame, text="<<", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        previous_label.grid(row=0, column=4, sticky='w')
        previous_label.bind("<Button-1>", lambda event, page_index=current_page-1: self.showUserProfilePages(page_index, all_profiles, all_profiles[page_index*12:(page_index*12)+12]) if page_index >= 0 else None)

        # next label
        next_label = tk.Label(btn_frame, text=">>", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        next_label.grid(row=0, column=4, sticky='e')
        next_label.bind("<Button-1>", lambda event, page_index=current_page+1: self.showUserProfilePages(page_index, all_profiles, all_profiles[page_index*12:(page_index*12)+12]) if page_index < page_number else None)


        for i,profile in enumerate(show_profiles):

            profile_frame = Frame(self.content_frame, bg='lightyellow')
            profile_frame.grid(row=i, column=0, sticky='nwse')

            # Labels
            number_label = tk.Label(profile_frame, text =i+1 , bg='lightyellow', fg='black', font=("", 13))
            number_label.place(relx=0.02, rely=0.5, anchor=tk.W)

            # Labels
            profile_label = tk.Label(profile_frame, text = profile.profileName , bg='lightyellow', fg='black', font=("", 13))
            profile_label.place(relx=0.05, rely=0.5, anchor=tk.W)

            if profile.suspend == 0 :
                suspend_status = 'active'
            elif profile.suspend == 1 :
                suspend_status = 'suspended'

            status_label = tk.Label(profile_frame, text=suspend_status, bg='lightyellow', fg='black', font=("", 13, "bold"))
            status_label.place(relx=0.25, rely=0.5, anchor=tk.W)

            view_button = tk.Button(profile_frame, text="View", bg='lightgrey', fg='black', font=("", 12),
                            command=lambda profileName=profile.profileName: self.openViewProfileInfoPage(profileName))
            view_button.place(relx=0.8, rely=0.5, anchor=tk.W, width=90)

            if profile.suspend == 0:
                suspend_button = tk.Button(profile_frame, text="Suspend", bg='lightgrey', fg='black', font=("", 12),
                                    command=lambda profileName=profile.profileName: self.openSuspendProfilePage(profileName))
                suspend_button.place(relx=0.9, rely=0.5, anchor=tk.W, width=90)
            elif profile.suspend == 1:
                unsuspend_button = tk.Button(profile_frame, text="Unsuspend", bg='lightgrey', fg='black', font=("", 12),
                                    command=lambda profileName=profile.profileName: self.openUnsuspendProfilePage(profileName))
                unsuspend_button.place(relx=0.9, rely=0.5, anchor=tk.W, width=90)

            # separator between home and average frames
            separator = ttk.Separator(profile_frame, orient='horizontal')
            separator.place(relx=0, rely=0.98, relwidth=1, relheight=0.02)

    def openManageAccountPage(self):
        self.home_frame.pack_forget()
        AdminManageUserAccountsPage(self.root, self.admin)

    def logout(self):
        UserLogoutPage(self.root, self.home_frame)

    def openSuspendProfilePage(self, profileId):
        AdminSuspendUserProfilePage(self.root, self.home_frame, self.admin, profileId)

    def openUnsuspendProfilePage(self, profileId):
        AdminUnsuspendUserProfilePage(self.root, self.home_frame, self.admin, profileId)

    def openCreateProfilePage(self, event):
        self.home_frame.pack_forget()
        AdminCreateUserProfilePage(self.root, self.admin)

    def openViewProfileInfoPage(self, userId):
        self.home_frame.pack_forget()
        AdminViewProfileInfoPage(self.root, self.admin, userId)

    def openCheckAccountPage(self):
        self.home_frame.pack_forget()
        UserViewMyAccountPage(self.root, self.admin.user_id, self.page)

class AdminCreateUserProfilePage:
    def __init__(self, root, admin):
        self.root = root
        self.admin = admin
        self.displayAdminCreateUserProfiePage()

    def displayAdminCreateUserProfiePage(self):
        
        # base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)
        
        # Title Label
        label_title = tk.Label(header_frame, text="Create Profile", bg="lightblue",  fg='black', font=("", 22, "bold"))
        label_title.pack(expand=True, anchor=tk.CENTER)
    
        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)
        
        # 2nd Frame
        self.content_frame = tk.Frame(self.home_frame, bg="white")
        self.content_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.9)# Allow content frame to expand
        
        # userId Label
        label_profileName = tk.Label(self.content_frame, text="Profile Name", bg="white", padx=10, pady=5, font=("", 15))
        label_profileName.place(relx=0.3, rely=0.1, anchor=tk.W)
        
        # Entry widget for userId
        self.entry_profileName = tk.Entry(self.content_frame, font=("", 15), bd=1, relief=tk.SOLID)
        self.entry_profileName.place(relx=0.55, rely=0.1, anchor=tk.W, relwidth=0.2)
        
        # Description 
        label_desc = tk.Label(self.content_frame, text="Description", bg="white", padx=10, pady=5, font=("", 15))
        label_desc.place(relx=0.3, rely=0.2, anchor=tk.W)
        
        # Text widget for description
        self.text_desc = tk.Text(self.content_frame, width=30, font=("", 15), height=12.5, bd=1, relief=tk.SOLID)
        self.text_desc.place(relx=0.55, rely=0.2, anchor=tk.NW, relwidth=0.2, relheight=0.4)

        #Cancel Button
        cancel_button = tk.Button(self.content_frame,  text="Cancel",  bg="lightgray", padx=10, pady=5, font=("", 12), command=self.openAdminHomePage)
        cancel_button.place(relx=0.47, rely=0.7, anchor=tk.E, width=100)
        
        create_button = tk.Button(self.content_frame,  text="Create",  bg="lightgray", padx=10, pady=5, font=("", 12), command=self.createUserProfile)
        create_button.place(relx=0.53, rely=0.7, anchor=tk.W, width=100)
    
    def createUserProfile(self):
        #get user input values
        profileName = self.entry_profileName.get()
        profileDescription = self.text_desc.get("1.0",'end-1c')
        suspend = 0

        #create instance of profile 
        self.newProfile = controller.AdminCreateUserProfileController()

        while True:
            if (profileName == "" or profileDescription == ""):
                messagebox.showerror("Failed","Please check that you have entered all details.")
                break
            else:
                result = self.newProfile.createUserProfile(profileName,profileDescription,suspend)
                if (result):
                    messagebox.showinfo("Successful", "New profile has been created.")
                else:
                    messagebox.showinfo("Failed", "Error creating account.")
                break
                
    def openAdminHomePage(self):
        self.home_frame.pack_forget()
        AdminManageUserProfilesPage(self.root, self.admin)

class AdminViewProfileInfoPage:
    def __init__(self, root, admin, profileName):
        self.root = root
        self.admin = admin
        self.profileName = profileName
        self.viewProfileInfo()


    def viewProfileInfo(self):
        self.viewProfileController = controller.AdminViewProfileInfoController()
        self.viewProfile = self.viewProfileController.viewProfileInfo(self.profileName)
 
        self.displayAdminViewProfileInfoPage()

    def displayAdminViewProfileInfoPage(self):

        # base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)
        
        # Title Label
        label_title = tk.Label(header_frame, text="View Profile", bg="lightblue",  fg='black', font=("", 22, "bold"))
        label_title.pack(expand=True, anchor=tk.CENTER)
    
        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)
        
        # 2nd Frame
        content_frame = tk.Frame(self.home_frame, bg="white")
        content_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.9)# Allow content frame to expand
        
        # profile Label
        label_profileName = tk.Label(content_frame, text="Profile Name", bg="white", padx=10, pady=5, font=("", 15))
        label_profileName.place(relx=0.3, rely=0.1, anchor=tk.W)

        # userId Label
        profileName = tk.Label(content_frame, text=self.viewProfile.profileName, bg="white", font=("", 15))
        profileName.place(relx=0.55, rely=0.1, anchor=tk.W)
        
        # Description 
        label_desc = tk.Label(content_frame, text="Description", bg="white", padx=10, pady=5, font=("", 15))
        label_desc.place(relx=0.3, rely=0.2, anchor=tk.W)
        
        #Text box
        text_desc = Text(content_frame, bg='white', borderwidth=0, relief="solid", font=("", 15))
        text_desc.insert(END, self.viewProfile.description)
        text_desc.configure(state='disabled')
        text_desc.place(relx=0.55, rely=0.2, anchor=tk.NW, relwidth=0.2, relheight=0.4)

        #Cancel Button
        self.cancel_button = tk.Button(content_frame,  text="Cancel",  bg="lightgray", padx=10, pady=5, font=("", 12), command=self.openAdminHomePage)
        self.cancel_button.place(relx=0.47, rely=0.7, anchor=tk.E, width=100)
        
        self.edit_button = tk.Button(content_frame,  text="Edit",  bg="lightgray", padx=10, pady=5, font=("", 12), command=self.openUpdateProfilePage)
        self.edit_button.place(relx=0.53, rely=0.7, anchor=tk.W, width=100)

    def openAdminHomePage(self):
        self.home_frame.pack_forget()
        AdminManageUserProfilesPage(self.root, self.admin)  

    def openUpdateProfilePage(self):
        if self.viewProfile.suspend == 1 :
            messagebox.showerror("Failed","The profile is suspended! Please Unsuspend First to update the information.")
        else:
            self.home_frame.pack_forget()
            AdminUpdateUserProfilePage(self.root, self.admin, self.viewProfile)  

class AdminUpdateUserProfilePage:
    def __init__(self, root, admin, profile):
        self.root = root
        self.admin = admin
        self.profile = profile
        self.displayAdminUpdateUserProfilePage()

    def displayAdminUpdateUserProfilePage(self):
        
        # base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)
        
        # Title Label
        label_title = tk.Label(header_frame, text="Update Profile", bg="lightblue",  fg='black', font=("", 22, "bold"))
        label_title.pack(expand=True, anchor=tk.CENTER)
    
        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)
        
        # 2nd Frame
        content_frame = tk.Frame(self.home_frame, bg="white")
        content_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.9)# Allow content frame to expand
        
        # userId Label
        label_profileName = tk.Label(content_frame, text="Profile Name", bg="white", padx=10, pady=5, font=("", 15))
        label_profileName.place(relx=0.3, rely=0.1, anchor=tk.W)

        # Entry widget for ProfileName
        self.entry_pName = tk.Label(content_frame, text=self.profile.profileName, bg="white", font=("", 15))
        self.entry_pName.place(relx=0.55, rely=0.1, anchor=tk.W)
        
        # Description 
        self.label_desc = tk.Label(content_frame, text="Description", bg="white", padx=10, pady=5, font=("", 15))
        self.label_desc.place(relx=0.3, rely=0.2, anchor=tk.W)

        # Text widget for description
        self.text_desc = tk.Text(content_frame, font=("", 15), height=12.5, bd=1, relief=tk.SOLID)
        self.text_desc.insert(END, self.profile.description)
        self.text_desc.place(relx=0.55, rely=0.2, anchor=tk.NW, relwidth=0.2, relheight=0.4)

        #Cancel Button
        self.cancel_button = tk.Button(content_frame,  text="Cancel",  bg="lightgray", padx=10, pady=5, font=("", 12), command=self.openAdminViewProfilePage)
        self.cancel_button.place(relx=0.47, rely=0.7, anchor=tk.E, width=100)
        
        self.update_button = tk.Button(content_frame,  text="Update",  bg="lightgray", padx=10, pady=5, font=("", 12), command=self.updateUserProfile)
        self.update_button.place(relx=0.53, rely=0.7, anchor=tk.W, width=100)
        
    def updateUserProfile(self):

        description = self.text_desc.get("1.0",'end-1c')
        #description = description[:-1]
        profileName = self.profile.profileName

        while (True):
            if (description == self.profile.description):
                messagebox.showerror("Failed","Fill the new data in order to update")
                break
            else:
                self.updateProfileController = controller.AdminUpdateUserProfileController()
                result = self.updateProfileController.updateUserProfile(profileName,description)
                if (result):
                    messagebox.showinfo("Successful!", "User Profile is updated successfully!")
                else:
                    messagebox.showerror("Failed!", "Updating user profile is failed!")
                break

    def openAdminViewProfilePage(self):
        self.home_frame.pack_forget()
        AdminViewProfileInfoPage(self.root, self.admin, self.profile.profileName)    

class AdminSuspendUserProfilePage:
    def __init__(self, root, passedFrame, admin, profileName):
        self.admin = admin
        self.root = root
        self.passedFrame = passedFrame
        self.profileName = profileName

        self.root1 = Tk()
        self.root1.geometry("450x250")
        self.root1.title("Suspend Profile")
        self.displayAdminSuspendUserProfilePage()

    def displayAdminSuspendUserProfilePage(self):
        
        # Create a frame to hold the suspend content
        suspend_frame = Frame(self.root1, bg="lightgrey")
        suspend_frame.pack(expand=True, fill=BOTH, padx=20, pady=20)

        # Create a boundary rectangle shape
        canvas = Canvas(suspend_frame, bg="white", highlightthickness=0)
        canvas.pack(expand=True, fill=tk.BOTH)

        # Draw the boundary rectangle
        canvas.create_rectangle(10, 10, 390, 190, outline="black", width=2)

        # Add the text
        canvas.create_text(200, 60, text=f"You are about to suspend {self.profileName}", font=("", 12), justify=CENTER)
        canvas.create_text(200, 90, text="Are you sure?", font=("", 12), justify=CENTER)

        # Create buttons
        cancel_button = Button(suspend_frame, text="Cancel", command=self.cancel_suspend)
        suspend_button = Button(suspend_frame, text="Suspend", command=self.confirm_suspend)

        # Add buttons inside the rectangle boundary
        canvas.create_window(140, 140, window=cancel_button)
        canvas.create_window(260, 140, window=suspend_button)

    def cancel_suspend(self):
        # Close the suspend page without logging out
        self.root1.destroy()

    def confirm_suspend(self):
        # Create an instance of SuspendUserController
        suspend_controller = controller.AdminSuspendUserProfileController()
        # Call the suspend_user method to suspend the user by userID
        success = suspend_controller.suspendUserProfile(self.profileName)
        
        # Check if suspension was successful
        if success:
            messagebox.showinfo("Suspend Success", f"{self.profileName}' profile suspended successfully.")
        else:
            messagebox.showerror("Suspend Failed",f"'{self.profileName}' profile is already suspended.")

        # Close the application or perform other actions here
        self.root1.destroy()
        self.passedFrame.pack_forget()
        AdminManageUserProfilesPage(self.root, self.admin)

class AdminUnsuspendUserProfilePage:
    def __init__(self, root, passedFrame, admin, profileName):
        self.admin = admin
        self.root = root
        self.passedFrame = passedFrame
        self.profileName = profileName

        self.root1 = Tk()
        self.root1.geometry("450x250")
        self.root1.title("Unuspend Profile")
        self.displayAdminUnsuspendUserProfilePage()

    def displayAdminUnsuspendUserProfilePage(self):
        
        # Create a frame to hold the suspend content
        unsuspend_frame = Frame(self.root1, bg="lightgrey")
        unsuspend_frame.pack(expand=True, fill=BOTH, padx=20, pady=20)

        # Create a boundary rectangle shape
        canvas = Canvas(unsuspend_frame, bg="white", highlightthickness=0)
        canvas.pack(expand=True, fill=tk.BOTH)

        # Draw the boundary rectangle
        canvas.create_rectangle(10, 10, 390, 190, outline="black", width=2)

        # Add the text
        canvas.create_text(200, 60, text=f"You are about to unsuspend {self.profileName}", font=("", 12), justify=CENTER)
        canvas.create_text(200, 90, text="Are you sure?", font=("", 12), justify=CENTER)

        # Create buttons
        cancel_button = Button(unsuspend_frame, text="Cancel", command=self.cancel_unsuspend)
        unsuspend_button = Button(unsuspend_frame, text="Unuspend", command=self.confirm_unsuspend)

        # Add buttons inside the rectangle boundary
        canvas.create_window(140, 140, window=cancel_button)
        canvas.create_window(260, 140, window=unsuspend_button)

    def cancel_unsuspend(self):
        # Close the suspend page without logging out
        self.root1.destroy()

    def confirm_unsuspend(self):
        # Create an instance of SuspendUserController
        unsuspend_controller = controller.AdminUnsuspendUserProfileController()
        # Call the suspend_user method to suspend the user by userID
        success = unsuspend_controller.unsuspendUserProfile(self.profileName)
        
        # Check if suspension was successful
        if success:
            messagebox.showinfo("Unsuspend Success", f" '{self.profileName}' profile unsuspended successfully.")
        else:
            messagebox.showerror("Unsuspend Failed",f"'{self.profileName}' is already active.")

        # Close the application or perform other actions here
        self.root1.destroy()
        self.passedFrame.pack_forget()
        AdminManageUserProfilesPage(self.root, self.admin)

class AgentHomePage: 
    def __init__(self, root, agent):
        self.root = root
        self.agent = agent
        self.page = 'agent'
        self.displayAgentHomePage()

    def displayAgentHomePage(self):

        # base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)

        # Agent ID label 
        agent_label = tk.Label(header_frame, text=self.agent.userName, bg='lightblue', fg='black', font=("", 18, "bold"))
        agent_label.pack(side='left', padx=20, pady=10)
        
        # Logout Button
        logout_btn = Button(header_frame, text=' Logout ', bg='lightyellow', font=("", 14, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.logout)
        logout_btn.pack(side='right', padx=20, pady=10)

        # Profile Button
        profile_btn = Button(header_frame, text=' Profile ', bg='lightyellow', font=("", 14, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.openCheckAccountPage)
        profile_btn.pack(side='right', pady=10)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # menu frame
        menu_frame = Frame(self.home_frame, bg='white')
        menu_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.075)

        # new label
        all_listings_label = tk.Label(menu_frame, text="All Listings", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        all_listings_label.pack(side='left', padx=(20,20), pady=10)
        all_listings_label.bind("<Button-1>", lambda event: self.viewAllPropertyListings())

        # line seprator
        line = tk.Frame(menu_frame, bg='black', width=2, height=40)
        line.pack(side='left', pady=10)

        # sold label
        my_listing_label = tk.Label(menu_frame, text="My Listings", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        my_listing_label.pack(side='left', padx=(20,20), pady=10)
        my_listing_label.bind("<Button-1>", lambda event: self.viewMyPropertyListings())

        # create label
        create_label = tk.Label(menu_frame, text="Create", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        create_label.pack(side='right', padx=(20,20), pady=10)
        create_label.bind("<Button-1>", lambda event: self.openCreateListingPage())

        # line seprator
        line = tk.Frame(menu_frame, bg='black', width=2, height=40)
        line.pack(side='right', pady=10)

         # review label
        review_label = tk.Label(menu_frame, text="Review", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        review_label.pack(side='right', padx=(20,20), pady=10)
        review_label.bind("<Button-1>", lambda event: self.openReviewPage())

        # line seprator
        line = tk.Frame(menu_frame, bg='black', width=2, height=40)
        line.pack(side='right', pady=10)

         # review label
        review_label = tk.Label(menu_frame, text="Rating", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        review_label.pack(side='right', padx=(20,20), pady=10)
        review_label.bind("<Button-1>", lambda event: self.openRatingPage())

        # Search Listing
        self.search_entry = tk.Entry(menu_frame, bg='white', fg='black', font=("", 13))
        self.search_entry.insert(0, "Search Listing")
        self.search_entry.bind("<FocusIn>", lambda e: self.search_entry.delete(0, "end"))
        self.search_entry.pack(side='right', pady=10, padx=20)
        self.search_entry.bind("<Return>", self.searchPropertyListing)

        # separator between home and average frames
        separator2 = ttk.Separator(menu_frame, orient='horizontal')
        separator2.place(relx=0, rely=0.98, relwidth=1, relheight=0.02)

        # listing Main Frame
        self.listing_frame = Frame(self.home_frame, bg='lightyellow')
        self.listing_frame.place(relx=0, rely=0.175, relwidth=1, relheight=0.825)

        self.viewAllPropertyListings()

    def viewAllPropertyListings(self):
        listings = controller.AgentViewAllPropertyListingsController()
        self.allListings = listings.viewAllPropertyListings()        
        self.showListingPages(0, 'All', self.allListings, self.allListings[0:12])
    
    def viewMyPropertyListings(self):
        listings = controller.AgentViewMyPropertyListingsController()
        self.allListings = listings.viewMyPropertyListings(self.agent.user_id)        
        self.showListingPages(0, 'My', self.allListings, self.allListings[0:12])

    def searchPropertyListing(self, event):

        Searchlisting = self.search_entry.get()

        if (Searchlisting == ""):
            messagebox.showerror("Failed", "Please enter a name to search")
            return self.viewAllPropertyListings()

        listings = controller.SearchPropertyListingController()
        self.Searchedlistings = listings.searchPropertyListing(Searchlisting)
        self.showListingPages(0, 'Searched', self.Searchedlistings, self.Searchedlistings[0:12])

    def showListingPages(self, page_index, heading, all_listing, show_listing):

        for widget in self.listing_frame.winfo_children():
            widget.destroy()

        # label frame for public listing 
        label_frame = Frame(self.listing_frame, bg='lightyellow')
        label_frame.place(relx=0, rely=0, relwidth=1, relheight=0.075)

        # public_listing label 
        label = tk.Label(label_frame, text=f"{heading} Listing", bg='lightyellow', fg='black', font=("", 14, "bold"))
        label.pack(side='bottom',pady=5)

        # content frame for public listing 
        content_frame = Frame(self.listing_frame, bg='lightyellow')
        content_frame.place(relx=0, rely=0.075, relwidth=1, relheight=0.925)

        current_page = page_index

        if len(all_listing) == 0:
            page_number = 1
        elif len(all_listing) % 12 == 0:
            page_number = int(len(all_listing)/12)  
        else:
            page_number = int(len(all_listing)/12) + 1
        

        image_frame1 = Frame(content_frame, bg='lightyellow')
        image_frame1.place(relx=0, rely=0, relwidth=1, relheight=0.4)
        image_frame1.rowconfigure((0), weight=1, uniform='a')
        image_frame1.columnconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        title_frame1 = Frame(content_frame, bg='lightyellow')
        title_frame1.place(relx=0, rely=0.4, relwidth=1, relheight=0.05)
        title_frame1.rowconfigure((0), weight=1, uniform='a')
        title_frame1.columnconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        image_frame2 = Frame(content_frame, bg='lightyellow')
        image_frame2.place(relx=0, rely=0.45, relwidth=1, relheight=0.4)
        image_frame2.rowconfigure((0), weight=1, uniform='a')
        image_frame2.columnconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        title_frame2 = Frame(content_frame, bg='lightyellow')
        title_frame2.place(relx=0, rely=0.85, relwidth=1, relheight=0.05)
        title_frame2.rowconfigure((0), weight=1, uniform='a')
        title_frame2.columnconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        btn_frame = Frame(content_frame, bg='lightyellow')
        btn_frame.place(relx=0, rely=0.9, relwidth=1, relheight=0.1)
        btn_frame.rowconfigure((0), weight=1, uniform='a')
        btn_frame.columnconfigure((0,1,2,3,4,5,6,7,8), weight=1, uniform='a')

        # number label
        number_label = tk.Label(btn_frame, text=f"{current_page+1}/{page_number}", bg='lightyellow', fg='black', font=("", 13))
        number_label.grid(row=0, column=4, sticky='nwse')

        # previous label
        previous_label = tk.Label(btn_frame, text="<<", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        previous_label.grid(row=0, column=4, sticky='w')
        previous_label.bind("<Button-1>", lambda event, page_index=current_page-1: self.showListingPages(page_index, heading, all_listing, all_listing[page_index*12:(page_index*12)+12]) if page_index >= 0 else None)

        # previous label
        next_label = tk.Label(btn_frame, text=">>", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        next_label.grid(row=0, column=4, sticky='e')
        next_label.bind("<Button-1>", lambda event, page_index=current_page+1: self.showListingPages(page_index, heading, all_listing, all_listing[page_index*12:(page_index*12)+12]) if page_index < page_number else None)

        for i,listing in enumerate(show_listing):

            if i < 6:
                frame1 = Frame(image_frame1, bg='lightyellow')
                frame1.grid(row=0, column=i, sticky='nwse')

                img = Image.open(listing.img)
                img_resized = img.resize((150,150), Image.LANCZOS)
                tk_image = ImageTk.PhotoImage(img_resized)
                image_label = tk.Label(frame1, image=tk_image, bg='lightyellow', cursor='hand2', highlightthickness=1, highlightbackground='black')
                image_label.image = tk_image  # Store reference to image to prevent garbage collection
                image_label.pack(side='top', expand=True)
                image_label.bind("<Button-1>", lambda event, selectedListing = listing.listing_id: self.openViewListingInfo(selectedListing))

                label1 = Label(title_frame1, text=listing.title, bg='lightyellow', fg='black', font=("", 13))
                label1.grid(row=0, column=i, sticky='n')

            else:
                frame1 = Frame(image_frame2, bg='lightyellow')
                frame1.grid(row=0, column=i%6, sticky='nwse')

                img = Image.open(listing.img)
                img_resized = img.resize((150,150), Image.LANCZOS)
                tk_image = ImageTk.PhotoImage(img_resized)
                image_label = tk.Label(frame1, image=tk_image, bg='lightyellow', cursor='hand2', highlightthickness=1, highlightbackground='black')
                image_label.image = tk_image  # Store reference to image to prevent garbage collection
                image_label.pack(side='top', expand=True)
                image_label.bind("<Button-1>", lambda event, selectedListing = listing.listing_id: self.openViewListingInfo(selectedListing))

                label1 = Label(title_frame2, text=listing.title, bg='lightyellow', fg='black', font=("", 13))
                label1.grid(row=0, column=i%6, sticky='n')
                #label1.pack(side='top', expand=True, pady=20)

    def openViewListingInfo(self, selectedListing):
        self.home_frame.pack_forget()
        AgentViewListingInfoPage(self.root, self.agent, selectedListing)

    def openCreateListingPage(self):
        self.home_frame.pack_forget()
        AgentCreatePropertyListingPage(self.root,self.agent)

    def openCheckAccountPage(self):
        self.home_frame.pack_forget()
        UserViewMyAccountPage(self.root, self.agent.user_id, self.page)

    def logout(self):
        UserLogoutPage(self.root, self.home_frame)

    def openRatingPage(self):
        self.home_frame.pack_forget()
        AgentViewRatingPage(self.root, self.agent)

    def openReviewPage(self):
        self.home_frame.pack_forget()
        AgentViewReviewPage(self.root, self.agent)

class AgentCreatePropertyListingPage:
    def __init__(self, root, agent):
        self.root = root
        self.agent = agent
        self.filename = "images/home.png"
        self.displayAgentCreatePropertyListingPage()
       
    def displayAgentCreatePropertyListingPage(self):

        #base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)

        # Title Label
        label_title = tk.Label(header_frame, text="Create Listing", bg="lightblue",  fg='black', font=("", 22, "bold"))
        label_title.pack(expand=True, anchor=tk.CENTER)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # 2nd Frame
        content_frame = tk.Frame(self.home_frame, bg="white")
        content_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.9)# Allow content frame to expand

        #Image
        # Load and resize the image
        image_path = "images/plus.png"
        image = Image.open(image_path)  # Replace with the actual image path
        image = image.resize((200, 200)) 
        photo = ImageTk.PhotoImage(image)

        # Create a label to display the image
        self.image_label = Label(content_frame, image=photo, bg='white', cursor='hand2', highlightthickness=1, highlightbackground='black')
        self.image_label.image = photo 
        self.image_label.place(relx=0.2, rely=0.05, relwidth= 0.16, relheight=0.32)
        self.image_label.bind("<Button-1>", self.browse_image)

        # Title Label
        label_Title = Label(content_frame, text="Title" , bg="white", padx=10, pady=5, font=("", 15))
        label_Title.place(relx=0.45, rely=0.1, anchor=W)

        self.entry_Title = Entry(content_frame, width=30, bd=1, relief=SOLID, font=("", 15))
        self.entry_Title.place(relx=0.55, rely=0.1, anchor=W, relwidth=0.25)

        # Status Label
        label_Status = Label(content_frame, text="Status", bg="white", padx=10, pady=5, font=("", 15))
        label_Status.place(relx=0.45, rely=0.175, anchor=W)

        self.dropdown_Status = ttk.Combobox(content_frame, values=["Available", "Sold"], width=27, state="readonly", font=("", 15))
        self.dropdown_Status.current(0)  # Set the default option
        self.dropdown_Status.place(relx=0.55, rely=0.175, anchor=W, relwidth=0.25)

        # Seller Label
        label_Seller = Label(content_frame, text="Seller", bg="white", padx=10, pady=5, font=("", 15))
        label_Seller.place(relx=0.45, rely=0.25, anchor=W)

        self.entry_Seller= Entry(content_frame, width=30, bd=1, relief=SOLID,font=("", 15))
        self.entry_Seller.place(relx=0.55, rely=0.25, anchor=W, relwidth=0.25)

        # Price Label
        label_price = Label(content_frame, text="Price", bg="white", padx=10, pady=5, font=("", 15))
        label_price.place(relx=0.45, rely=0.325, anchor=tk.W)

        self.entry_price  = Entry(content_frame, width=30, bd=1, relief=SOLID, font=("", 15))
        self.entry_price.place(relx=0.55, rely=0.325, anchor=W, relwidth=0.25)

        label_description = Label(content_frame, text="Description: ", bg="white", font=("", 15))
        label_description.place(relx=0.2, rely=0.425, anchor=W)

        #Text box
        self.large_textbox = Text(content_frame, borderwidth=1, relief="solid", font=("", 12))
        self.large_textbox.place(relx=0.2, rely=0.45, anchor=NW, relwidth=0.6, relheight=0.4)

        # Button
        cancel_button = Button(content_frame,  text="Cancel",  bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.openAgentHomePage)#, 
        cancel_button.place(relx=0.47, rely=0.9, anchor=E, width=100)

        create_button = tk.Button(content_frame,  text="Create",  bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.createPropertyListing)
        create_button.place(relx=0.53, rely=0.9, anchor=W, width=100)

    def createPropertyListing(self):

        title = self.entry_Title.get()
        status = self.dropdown_Status.get()
        agent_id = self.agent.user_id
        seller_id  = self.entry_Seller.get()
        price = self.entry_price.get()
        description = self.large_textbox.get("1.0",'end-1c')
        suspend = 0 

        pricePattern = r'^[0-9]*\.?[0-9]+$'

        while (True):
            
            if title == "":
                messagebox.showinfo("Failed", "Please enter a title")
                break
            elif seller_id == "":
                messagebox.showinfo("Failed", "Please enter a valid SellerID")
                break
            elif re.match(pricePattern,price) is None:
                messagebox.showinfo("Failed", "You must put number only for price")
                break
            elif description == "":
                messagebox.showinfo("Failed", "Please enter a description")
                break

            self.save_image()
            img = self.image_path

            createListing = controller.AgentCreatePropertyListingController()
            result = createListing.createPropertyListing(title, status, img, agent_id, seller_id, price ,description, suspend)

            if(result):
                # Copy the selected image file to your local directory
                shutil.copyfile(self.filename, self.image_path)
                messagebox.showinfo("Successful", "New listing has been created.")
            else:
                messagebox.showerror("Failed","Could not find seller in the system")
            break

    def browse_image(self, event):
        # Open the file dialog to select an image
        filename = filedialog.askopenfilename(initialdir="/", title="Select an Image", filetypes=(("Image files", "*.jpg; *.jpeg; *.png"), ("All files", "*.*")))

        if filename:
            self.filename = filename

        image = Image.open(self.filename)
        image = image.resize((200, 200))
        photo = ImageTk.PhotoImage(image)

        # Update the label to display the selected image
        self.image_label.configure(image=photo)
        self.image_label.image = photo  # Keep a reference to avoid garbage collection

    def save_image(self):
        # saving image
        base_name = os.path.basename(self.filename)
        name , ext = os.path.splitext(base_name)

        # Determine the destination directory
        dst_dir = "listings"
        # Find the current count of images
        count = len([name for name in os.listdir(dst_dir)])
        # Determine the new file name
        new_filename = f"{count + 1}.{ext[1:]}"
        dst_dir = dst_dir.replace('\\', '/')
        dst_path = os.path.join(dst_dir, new_filename)

        self.image_path = dst_path


    def openAgentHomePage(self):
        self.home_frame.pack_forget()
        AgentHomePage(self.root, self.agent)

class AgentViewListingInfoPage:
    def __init__(self, root, agent, listing_id):
        self.root = root
        self.agent = agent
        self.listing_id = listing_id
        self.viewListingInfo()

    def viewListingInfo(self):

        viewPropertyListingController = controller.AgentViewListingInfoController()
        self.propertyListing = viewPropertyListingController.viewListingInfo(self.listing_id)
 
        self.displayAgentViewListingInfoPage()

    def displayAgentViewListingInfoPage(self):

        #base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)

        # Title Label
        label_title = tk.Label(header_frame, text="View Listing", bg="lightblue",  fg='black', font=("", 22, "bold"))
        label_title.pack(expand=True, anchor=tk.CENTER)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # 2nd Frame
        content_frame = tk.Frame(self.home_frame, bg="white")
        content_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.9)# Allow content frame to expand

        #Image
        # Load and resize the image
        image_path = self.propertyListing.img
        image = Image.open(image_path)  # Replace with the actual image path
        image = image.resize((200, 200)) 
        photo = ImageTk.PhotoImage(image)

        # Create a label to display the image
        self.image_label = Label(content_frame, image=photo, bg='white', cursor='hand2', highlightthickness=1, highlightbackground='black')
        self.image_label.image = photo 
        self.image_label.place(relx=0.2, rely=0.05, relwidth= 0.16, relheight=0.32)

        # Title Label
        label_Title = Label(content_frame, text="Title" , bg="white", padx=10, pady=5, font=("", 15))
        label_Title.place(relx=0.45, rely=0.1, anchor=W)

        # Title Label
        title = Label(content_frame, text=self.propertyListing.title , bg="white", font=("", 15))
        title.place(relx=0.55, rely=0.1, anchor=W)

        # Status Label
        label_Status = Label(content_frame, text="Status", bg="white", padx=10, pady=5, font=("", 15))
        label_Status.place(relx=0.45, rely=0.175, anchor=W)

        status = Label(content_frame, text=self.propertyListing.status, bg="white", font=("", 15))
        status.place(relx=0.55, rely=0.175, anchor=W)


        if (self.agent.user_id == self.propertyListing.agent_id):
            # Seller Label
            label_Seller = Label(content_frame, text="Seller", bg="white", padx=10, pady=5, font=("", 15))
            label_Seller.place(relx=0.45, rely=0.25, anchor=W)
            
            seller = Label(content_frame, text=self.propertyListing.seller_id, bg="white", font=("", 15))
            seller.place(relx=0.55, rely=0.25, anchor=W)
        else:
            # Seller Label
            label_Agent = Label(content_frame, text="Agent", bg="white", padx=10, pady=5, font=("", 15))
            label_Agent.place(relx=0.45, rely=0.25, anchor=tk.W)

            Agent = Label(content_frame, text=self.propertyListing.agent_name, bg="white", font=("", 15))
            Agent.place(relx=0.55, rely=0.25, anchor=tk.W)

        # Price Label
        label_price = Label(content_frame, text="Price", bg="white", padx=10, pady=5, font=("", 15))
        label_price.place(relx=0.45, rely=0.325, anchor=tk.W)

        # Price
        price = Label(content_frame, text=self.propertyListing.price, bg="white", font=("", 15))
        price.place(relx=0.55, rely=0.325, anchor=tk.W)

        label_description = Label(content_frame, text="Description: ", bg="white", font=("", 15))
        label_description.place(relx=0.2, rely=0.425, anchor=W)

        #Text box
        large_textbox = Text(content_frame, bg="white", borderwidth=0, relief="solid", font=("", 13))
        large_textbox.insert(END, self.propertyListing.description)
        large_textbox.configure(state='disabled')
        large_textbox.place(relx=0.2, rely=0.45, anchor=NW, relwidth=0.6, relheight=0.3)

        #View Count
        label_view = Label(content_frame, text="View Count: " + str(self.propertyListing.view_count), bg="white", padx=10, pady=5, font=("", 15))
        label_view.place(relx=0.2, rely=0.8, anchor=tk.W)
            
        label_shortlist = Label(content_frame, text="Shorlist Count: " + str(self.propertyListing.shortlist_count), bg="white", padx=10, pady=5, font=("", 15))
        label_shortlist.place(relx=0.8, rely=0.8, anchor=tk.E)

        if (self.agent.user_id == self.propertyListing.agent_id):
            # Cancel Button
            cancel_button = tk.Button(content_frame,  text="Cancel",   bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.openAgentHomePage)
            cancel_button.place(relx=0.3, rely=0.9, anchor=tk.CENTER, width=100)

            # Edit Button
            edit_button = Button(content_frame,  text="Edit",  bg="lightgrey", padx=10, pady=5, font=("", 12), command = self.openUpdateListing)
            edit_button.place(relx=0.5, rely=0.9, anchor=tk.CENTER, width=100)
        
            if (self.propertyListing.suspend == 0):
                suspend_button = Button(content_frame,  text="Suspend",  bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.openSuspendListingPage)
                suspend_button.place(relx=0.7, rely=0.9, anchor=tk.CENTER, width=100)
            else:
                unsuspend_button = Button(content_frame,  text="Unuspend",  bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.openUnsuspendListingPage)
                unsuspend_button.place(relx=0.7, rely=0.9, anchor=tk.CENTER, width=100)
        else: 
            cancel_button = tk.Button(content_frame,  text="Cancel",   bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.openAgentHomePage)
            cancel_button.place(relx=0.5, rely=0.9, anchor=tk.CENTER, width=100)

    def openAgentHomePage(self):
        self.home_frame.pack_forget()
        AgentHomePage(self.root, self.agent)

    def openUpdateListing(self):
        if self.propertyListing.suspend == 1 :
            messagebox.showerror("Failed","The listing is suspended! Please Unsuspend First to update the information.")
        else:
            self.home_frame.pack_forget()
            AgentUpdatePropertyListingPage(self.root, self.agent, self.propertyListing)
        
    def openSuspendListingPage(self):
        AgentSuspendPropertyListingPage(self.root,self.home_frame,self.agent,self.propertyListing.listing_id, self.propertyListing.title)

    def openUnsuspendListingPage(self):
        AgentUnsuspendPropertyListingPage(self.root,self.home_frame,self.agent,self.propertyListing.listing_id, self.propertyListing.title)

class AgentUpdatePropertyListingPage:
    def __init__(self, root, agent, listing):
        self.root = root
        self.agent = agent
        self.listing = listing
        self.filename = self.listing.img
        self.displayAgentUpdatePropertyListingPage()

    def displayAgentUpdatePropertyListingPage(self):
        #base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)

        # Title Label
        label_title = tk.Label(header_frame, text="Update Listing", bg="lightblue",  fg='black', font=("", 22, "bold"))
        label_title.pack(expand=True, anchor=tk.CENTER)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # 2nd Frame
        content_frame = tk.Frame(self.home_frame, bg="white")
        content_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.9)# Allow content frame to expand

        #Image
        # Load and resize the image
        image_path = self.listing.img
        image = Image.open(image_path)  # Replace with the actual image path
        image = image.resize((200, 200)) 
        photo = ImageTk.PhotoImage(image)

        # Create a label to display the image
        self.image_label = Label(content_frame, image=photo, bg='white', cursor='hand2', highlightthickness=1, highlightbackground='black')
        self.image_label.image = photo 
        self.image_label.place(relx=0.2, rely=0.05, relwidth= 0.16, relheight=0.32)
        self.image_label.bind("<Button-1>", self.browse_image)

        # Title Label
        label_Title = Label(content_frame, text="Title" , bg="white", padx=10, pady=5, font=("", 15))
        label_Title.place(relx=0.45, rely=0.1, anchor=W)

        self.entry_Title = Entry(content_frame, width=30, bd=1, relief=SOLID, font=("", 15))
        self.entry_Title.insert(0,self.listing.title)
        self.entry_Title.place(relx=0.55, rely=0.1, anchor=W, relwidth=0.25)

        # Status Label
        label_Status = Label(content_frame, text="Status", bg="white", padx=10, pady=5, font=("", 15))
        label_Status.place(relx=0.45, rely=0.175, anchor=W)

        self.dropdown_Status = ttk.Combobox(content_frame, values=["Available", "Sold"], width=27, state="readonly", font=("", 15))
        self.dropdown_Status.set(self.listing.status)
        self.dropdown_Status.place(relx=0.55, rely=0.175, anchor=W, relwidth=0.25)

        # Seller Label
        label_Seller = Label(content_frame, text="Seller", bg="white", padx=10, pady=5, font=("", 15))
        label_Seller.place(relx=0.45, rely=0.25, anchor=W)

        self.entry_Seller= Entry(content_frame, width=30, bd=1, relief=SOLID,font=("", 15))
        self.entry_Seller.insert(0,self.listing.seller_id)
        self.entry_Seller.place(relx=0.55, rely=0.25, anchor=W, relwidth=0.25)

        # Price Label
        label_price = Label(content_frame, text="Price", bg="white", padx=10, pady=5, font=("", 15))
        label_price.place(relx=0.45, rely=0.325, anchor=tk.W)

        self.entry_price  = Entry(content_frame, width=30, bd=1, relief=SOLID, font=("", 15))
        self.entry_price.insert(0,self.listing.price)
        self.entry_price.place(relx=0.55, rely=0.325, anchor=W, relwidth=0.25)

        label_description = Label(content_frame, text="Description: ", bg="white", font=("", 15))
        label_description.place(relx=0.2, rely=0.425, anchor=W)

        #Text box
        self.large_textbox = Text(content_frame, borderwidth=1, relief="solid", font=("", 12))
        self.large_textbox.insert('1.0',self.listing.description)
        self.large_textbox.place(relx=0.2, rely=0.45, anchor=NW, relwidth=0.6, relheight=0.4)

        # Button
        cancel_button = Button(content_frame,  text="Cancel",  bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.openViewListingPage)#, 
        cancel_button.place(relx=0.47, rely=0.9, anchor=E, width=100)

        update_button = tk.Button(content_frame,  text="Update",  bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.updatePropertyListing)
        update_button.place(relx=0.53, rely=0.9, anchor=W, width=100)

    def updatePropertyListing(self):

        title = self.entry_Title.get()
        status = self.dropdown_Status.get()
        seller_id = self.entry_Seller.get()
        price = self.entry_price.get()
        description = self.large_textbox.get("1.0",'end-1c')
        listing_id = self.listing.listing_id

        pricePattern = r'^[0-9]*\.?[0-9]+$'
            
        while(True):
            if ((title == "") or (status == "") or (seller_id == "") or (price == "") or (description == "")):
                messagebox.showerror("Failed","Please fill all empty fields to update.")
                break
            elif re.match(pricePattern,price) is None:
                messagebox.showinfo("Failed", "You must put number only for price")
                break
            elif ((title == self.listing.title) and (status == self.listing.status) and (seller_id == self.listing.seller_id) and 
            (int(price) == self.listing.price) and (description == self.listing.description) and (self.filename == self.listing.img)):
                messagebox.showerror("Failed","Edit the data in order to update")
                break

            else:
                if (self.filename != self.listing.img):
                    shutil.copyfile(self.filename, self.listing.img)

                updateListingController = controller.AgentUpdatePropertyListingController()
                result = updateListingController.updatePropertyListing(listing_id, title, status, seller_id, price, description)

                if(result):
                    messagebox.showinfo("Successful", "Listing has been updated successfully!")
                else:
                    messagebox.showerror("Failed","could not find seller in the system!")
                break

    def browse_image(self, event):
        # Open the file dialog to select an image
        filename = filedialog.askopenfilename(initialdir="/", title="Select an Image", filetypes=(("Image files", "*.jpg; *.jpeg; *.png"), ("All files", "*.*")))

        if filename:
            self.filename = filename

        image = Image.open(self.filename)
        image = image.resize((200, 200))
        photo = ImageTk.PhotoImage(image)

        # Update the label to display the selected image
        self.image_label.configure(image=photo)
        self.image_label.image = photo  # Keep a reference to avoid garbage collection

    def openViewListingPage(self):
        self.home_frame.pack_forget()
        AgentViewListingInfoPage(self.root, self.agent, self.listing.listing_id)

    def openAgentHomePage(self):
        self.home_frame.pack_forget()
        AgentHomePage(self.root, self.agent)

class AgentSuspendPropertyListingPage:

    def __init__(self, root, passedFrame, agent, listing_id, title):
        self.agent = agent
        self.root = root
        self.passedFrame = passedFrame
        self.listing_id = listing_id
        self.title = title

        self.root1 = Tk()
        self.root1.geometry("450x250")
        self.root1.title("Suspend Listing")
        self.displayAgentSuspendPropertyListingPage()

    def displayAgentSuspendPropertyListingPage(self):

        # Create a frame to hold the suspend content
        suspend_frame = Frame(self.root1, bg="lightgrey")
        suspend_frame.pack(expand=True, fill=BOTH, padx=20, pady=20)

        # Create a boundary rectangle shape
        canvas = Canvas(suspend_frame, bg="white", highlightthickness=0)
        canvas.pack(expand=True, fill=tk.BOTH)

        # Draw the boundary rectangle
        canvas.create_rectangle(10, 10, 390, 190, outline="black", width=2)

        # Add the text
        canvas.create_text(200, 60, text=f"You are about to suspend {self.title}", font=("", 12), justify=CENTER)
        canvas.create_text(200, 90, text="Are you sure?", font=("", 12), justify=CENTER)

        # Create buttons
        cancel_button = Button(suspend_frame, text="Cancel", command=self.cancel_suspend)
        suspend_button = Button(suspend_frame, text="Suspend", command=self.confirm_suspend)

        # Add buttons inside the rectangle boundary
        canvas.create_window(140, 140, window=cancel_button)
        canvas.create_window(260, 140, window=suspend_button)

    def cancel_suspend(self):
        # Close the suspend page without logging out
        self.root1.destroy()

    def confirm_suspend(self):
        # Create an instance of SuspendUserController

        suspend_controller = controller.AgentSuspendPropertyListingController()   
        success = suspend_controller.suspendPropertyListing(self.listing_id)
        
        # Check if suspension was successful
        if success:
            messagebox.showinfo("Suspend Success", "Property Listing is suspended successfully.")
        else:
            messagebox.showerror("Suspend Failed","Property Listing is already suspended.")

        # Close the application or perform other actions here
        self.root1.destroy()
        self.passedFrame.pack_forget()
        AgentViewListingInfoPage(self.root, self.agent, self.listing_id)

class AgentUnsuspendPropertyListingPage:
    def __init__(self, root, passedFrame, agent, listing_id, title):
        self.agent = agent
        self.root = root
        self.passedFrame = passedFrame
        self.listing_id = listing_id
        self.title = title

        self.root1 = Tk()
        self.root1.geometry("450x250")
        self.root1.title("Unsuspend Listing")
        self.displayAgentUnsuspendPropertyListingPage()

    def displayAgentUnsuspendPropertyListingPage(self):
        
        # Create a frame to hold the suspend content
        unsuspend_frame = Frame(self.root1, bg="lightgrey")
        unsuspend_frame.pack(expand=True, fill=BOTH, padx=20, pady=20)

        # Create a boundary rectangle shape
        canvas = Canvas(unsuspend_frame, bg="white", highlightthickness=0)
        canvas.pack(expand=True, fill=tk.BOTH)

        # Draw the boundary rectangle
        canvas.create_rectangle(10, 10, 390, 190, outline="black", width=2)

        # Add the text
        canvas.create_text(200, 60, text=f"You are about to unsuspend {self.title}", font=("", 12), justify=CENTER)
        canvas.create_text(200, 90, text="Are you sure?", font=("", 12), justify=CENTER)

        # Create buttons
        cancel_button = Button(unsuspend_frame, text="Cancel", command=self.cancel_unsuspend)
        unsuspend_button = Button(unsuspend_frame, text="Unuspend", command=self.confirm_unsuspend)

        # Add buttons inside the rectangle boundary
        canvas.create_window(140, 140, window=cancel_button)
        canvas.create_window(260, 140, window=unsuspend_button)

    def cancel_unsuspend(self):
        # Close the suspend page without logging out
        self.root1.destroy()

    def confirm_unsuspend(self):
        # Create an instance of SuspendUserController
        unsuspend_controller = controller.AgentUnsuspendPropertyListingController()
        # Call the suspend_user method to suspend the user by userID
        success = unsuspend_controller.unsuspendPropertyListing(self.listing_id)
        
        # Check if unsuspension was successful
        if success:
            messagebox.showinfo("Unsuspend Success", "Property Listing is unsuspended successfully.")
        else:
            messagebox.showerror("Unsuspend Failed","Property Listing is already unsuspended.")

        # Close the application or perform other actions here
        self.root1.destroy()
        self.passedFrame.pack_forget()
        AgentViewListingInfoPage(self.root, self.agent, self.listing_id)

class AgentViewRatingPage:
    def __init__(self, root, agent):
        self.root = root
        self.agent = agent
        self.viewRatings()

    def viewRatings(self):
        viewRatingController = controller.AgentViewRatingController()
        self.ratings = viewRatingController.viewRatings(self.agent.user_id)

        if len(self.ratings) > 0:
            self.page_number = int(len(self.ratings)/10) if len(self.ratings) % 10 == 0 else int(len(self.ratings)/10) + 1
            self.average_rating = "{:.2f}".format(sum(rating.rating for rating in self.ratings) / len(self.ratings))
        else:
            self.page_number = 1
            self.average_rating = 0

        self.displayAgentViewRatingPage()

    def displayAgentViewRatingPage(self):

        # Create a header frame to contain the Agent ID label
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)
        header_frame.columnconfigure((0,1,2), uniform='a', weight=1)
        header_frame.rowconfigure((0), uniform='a', weight=1)

        # title label 
        title_label = tk.Label(header_frame, text='Your Rating', bg='lightblue', fg='black', font=("", 18, "bold"))
        title_label.grid(row=0, column=1, sticky='nwse')

        # Agent ID label 
        agent_label = tk.Label(header_frame, text=self.agent.userName, bg='lightblue', fg='black', font=("", 18, "bold"))
        agent_label.grid(row=0, column=0, sticky='w', padx=20)

        # Home Button
        home_btn = Button(header_frame, text='  Home  ', bg='lightyellow', font=("", 14, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.openAgentHomePage)
        home_btn.grid(row=0, column=2, sticky='e', padx=20)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # Average Frame 
        average_frame= Frame(self.home_frame, bg='white')
        average_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.075)

        # Average label
        average_label = tk.Label(average_frame, text=f"Your Average Rating is {self.average_rating}", bg='white', fg='black', font=("", 15, "bold"))
        average_label.pack(expand=True, anchor=tk.CENTER)

        # separator between average and rating frames
        separator2 = ttk.Separator(average_frame, orient='horizontal')
        separator2.place(relx=0, rely=0.98, relwidth=1, relheight=0.02)

        # rating Frame 
        self.rating_frame = Frame(self.home_frame, bg='lightyellow')
        self.rating_frame.place(relx=0, rely=0.175, relwidth=1, relheight=0.825)
        if len(self.ratings) > 0:
            self.showRatingPages(0, self.ratings, self.ratings[0:10])
        else:
            self.showRatingPages(0, self.ratings, self.ratings)

    def showRatingPages(self, page_index, list, showlist):

        #global current_page
        current_page = page_index

        for widget in self.rating_frame.winfo_children():
            widget.destroy()

        content_frame = Frame(self.rating_frame, bg='lightyellow')
        content_frame.place(relx=0, rely=0, relwidth=1, relheight=0.95)
        content_frame.rowconfigure((0,1,2,3,4,5,6,7,8,9), weight=1, uniform='a')
        content_frame.columnconfigure((0), weight=1, uniform='a')

        btn_frame = Frame(self.rating_frame, bg='lightyellow')
        btn_frame.place(relx=0, rely=0.95, relwidth=1, relheight=0.05)
        btn_frame.rowconfigure((0), weight=1, uniform='a')
        btn_frame.columnconfigure((0,1,2,3,4,5,6,7,8), weight=1, uniform='a')

        # number label
        number_label = tk.Label(btn_frame, text=f"{current_page+1}/{self.page_number}", bg='lightyellow', fg='black', font=("", 13))
        number_label.grid(row=0, column=4, sticky='nwse')

        # previous label
        previous_label = tk.Label(btn_frame, text="<<", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        previous_label.grid(row=0, column=4, sticky='w')
        previous_label.bind("<Button-1>", lambda event, page_index=current_page-1: self.showRatingPages(page_index, self.ratings, self.ratings[page_index*10:(page_index*10)+10]) if page_index >= 0 else None)

        # previous label
        next_label = tk.Label(btn_frame, text=">>", bg='lightyellow',  cursor='hand2', fg='black', font=("", 13))
        next_label.grid(row=0, column=4, sticky='e')
        next_label.bind("<Button-1>", lambda event, page_index=current_page+1: self.showRatingPages(page_index,  self.ratings, self.ratings[page_index*10:(page_index*10)+10]) if page_index < self.page_number else None)

        self.rating_text_dict = {
                1: 'Worse',
                2: 'Bad',
                3: 'Neutral',
                4: 'Good',
                5: 'Excellent'
            }
        if len(self.ratings) > 0:
            for i,rating in enumerate(showlist):
                frame1 = Frame(content_frame, bg='lightyellow')
                frame1.grid(row=i, column=0, sticky='nsew')

                rating_text = self.rating_text_dict.get(rating.rating, 'Unknown')

                label1 = Label(frame1, text=f"A {rating.customer_profile} rated you : {rating.rating}*, {rating_text}", bg='lightyellow', fg='black', font=("", 13))
                label1.pack(expand=True, anchor=W, padx=20, pady=10)

                separator = ttk.Separator(frame1, orient='horizontal')
                separator.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)
        else:
            frame1 = Frame(content_frame, bg='lightyellow')
            frame1.grid(row=0, column=0, sticky='nsew')

            label1 = Label(frame1, text="You have received no rating yet!", bg='lightyellow', fg='black', font=("", 15))
            label1.pack(expand=True, anchor=W, padx=20, pady=10)

    def openAgentHomePage(self):
        self.home_frame.pack_forget()
        AgentHomePage(self.root, self.agent)

class AgentViewReviewPage:
    def __init__(self, root, agent):
        self.root = root
        self.agent = agent
        self.viewAllReviews()

    def viewAllReviews(self):
        viewReviewController = controller.AgentViewReviewController()
        self.reviews = viewReviewController.viewReview(self.agent.user_id)
        if len(self.reviews) > 0:
            self.page_number = int(len(self.reviews)/6) if len(self.reviews) % 6 == 0 else int(len(self.reviews)/6) + 1
        else:
            self.page_number = 1
        self.displayAgentViewReviewPage()

    def displayAgentViewReviewPage(self):

        # Create a header frame to contain the Agent ID label
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)
        header_frame.columnconfigure((0,1,2), uniform='a', weight=1)
        header_frame.rowconfigure((0), uniform='a', weight=1)

        # title label 
        title_label = tk.Label(header_frame, text='Your Review', bg='lightblue', fg='black', font=("", 18, "bold"))
        title_label.grid(row=0, column=1, sticky='nwse')

        # Agent ID label 
        agent_label = tk.Label(header_frame, text=self.agent.userName, bg='lightblue', fg='black', font=("", 18, "bold"))
        agent_label.grid(row=0, column=0, sticky='w', padx=20)

        # Home Button
        home_btn = Button(header_frame, text='  Home  ', bg='lightyellow', font=("", 14, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.openAgentHomePage)
        home_btn.grid(row=0, column=2, sticky='e', padx=20)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # Average Frame 
        average_frame= Frame(self.home_frame, bg='white')
        average_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.075)

        # Average label
        average_label = tk.Label(average_frame, text=f"Your have received ({len(self.reviews)}) reviews in total", bg='white', fg='black', font=("", 15, "bold"))
        average_label.pack(expand=True, anchor=tk.CENTER)

        # separator between average and rating frames
        separator2 = ttk.Separator(average_frame, orient='horizontal')
        separator2.place(relx=0, rely=0.98, relwidth=1, relheight=0.02)

        # rating Frame 
        self.rating_frame = Frame(self.home_frame, bg='lightyellow')
        self.rating_frame.place(relx=0, rely=0.175, relwidth=1, relheight=0.825)

        if len(self.reviews) > 0:
            self.show_review_pages(0, self.reviews, self.reviews[0:6])
        else:
            self.show_review_pages(0, self.reviews, self.reviews)

    def show_review_pages(self, page_index, list, showlist):

        # global current_page
        current_page = page_index

        for widget in self.rating_frame.winfo_children():
            widget.destroy()

        content_frame = Frame(self.rating_frame, bg='lightyellow')
        content_frame.place(relx=0, rely=0, relwidth=1, relheight=0.95)
        content_frame.rowconfigure((0,1,2,3,4,5), weight=1, uniform='a')
        content_frame.columnconfigure((0), weight=1, uniform='a')

        btn_frame = Frame(self.rating_frame, bg='lightyellow')
        btn_frame.place(relx=0, rely=0.95, relwidth=1, relheight=0.05)
        btn_frame.rowconfigure((0), weight=1, uniform='a')
        btn_frame.columnconfigure((0,1,2,3,4,5,6,7,8), weight=1, uniform='a')

        # number label
        number_label = tk.Label(btn_frame, text=f"{current_page+1}/{self.page_number}", bg='lightyellow', fg='black', font=("", 13))
        number_label.grid(row=0, column=4, sticky='nwse')

        # previous label
        previous_label = tk.Label(btn_frame, text="<<", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        previous_label.grid(row=0, column=4, sticky='w')
        previous_label.bind("<Button-1>", lambda event, page_index=current_page-1: self.show_review_pages(page_index, self.reviews, self.reviews[page_index*6:(page_index*6)+6]) if page_index >= 0 else None)

        # previous label
        next_label = tk.Label(btn_frame, text=">>", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        next_label.grid(row=0, column=4, sticky='e')
        next_label.bind("<Button-1>", lambda event, page_index=current_page+1: self.show_review_pages(page_index,  self.reviews, self.reviews[page_index*6:(page_index*6)+6]) if page_index < self.page_number else None)

        if len(self.reviews) > 0:
            for i,review in enumerate(showlist):
                frame1 = Frame(content_frame, bg='lightyellow')
                frame1.grid(row=i, column=0, sticky='nsew')

                label1 = Label(frame1, text=f"A {review.customer_profile} reviewd you as:",  cursor='hand2', bg='lightyellow', fg='black', font=("", 13))
                label1.pack(side='left', padx=20, pady=5)

                #Text box
                textbox = Text(frame1, bg='lightyellow', borderwidth=0, relief="solid", cursor='hand2', font=("", 13))
                textbox.insert(END, review.review)
                textbox.configure(state='disabled')
                textbox.pack(side='top', pady=5)

                separator = ttk.Separator(frame1, orient='horizontal')
                separator.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        else:
            frame1 = Frame(content_frame, bg='lightyellow')
            frame1.grid(row=0, column=0, sticky='nsew')

            label1 = Label(frame1, text="You have received no review yet!", bg='lightyellow', fg='black', font=("", 15))
            label1.pack(expand=True, anchor=W, padx=20, pady=10)
            
    def openAgentHomePage(self):
        self.home_frame.pack_forget()
        AgentHomePage(self.root, self.agent)

class BuyerHomePage: 
    def __init__(self, root, buyer):
        self.root = root
        self.buyer = buyer
        self.pagename = 'buyer'
        self.displayBuyerHomePage()

    def displayBuyerHomePage(self):
        # base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)

        # Agent ID label 
        agent_label = tk.Label(header_frame, text=self.buyer.userName, bg='lightblue', fg='black', font=("", 18, "bold"))
        agent_label.pack(side='left', padx=20, pady=10)

        # Logout Button
        logout_btn = Button(header_frame, text=' Logout ', bg='lightyellow', font=("", 14, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.logout)
        logout_btn.pack(side='right', padx=20, pady=10)

        # Profile Button
        profile_btn = Button(header_frame, text=' Profile ', bg='lightyellow', font=("", 14, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.openCheckAccountPage)
        profile_btn.pack(side='right', pady=10)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # menu frame
        menu_frame = Frame(self.home_frame, bg='white')
        menu_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.075)

        # new label
        new_label = tk.Label(menu_frame, text="New", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        new_label.pack(side='left', padx=(20,20), pady=10)
        new_label.bind("<Button-1>", lambda event: self.viewAllNewPropertyListings())

        # line seprator
        line = tk.Frame(menu_frame, bg='black', width=2, height=40)
        line.pack(side='left', pady=10)

        # sold label
        sold_label = tk.Label(menu_frame, text="Sold", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        sold_label.pack(side='left', padx=(20,20), pady=10)
        sold_label.bind("<Button-1>", lambda event: self.viewAllSoldPropertyListings())

        # line seprator
        line = tk.Frame(menu_frame, bg='black', width=2, height=40)
        line.pack(side='left', pady=10)

        # sold label
        saved_label = tk.Label(menu_frame, text="Saved", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        saved_label.pack(side='left', padx=(20,20), pady=10)
        saved_label.bind("<Button-1>", lambda event: self.viewSavedPropertyListings())

        # create label
        calculate_label = tk.Label(menu_frame, text="Calculate", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        calculate_label.pack(side='right', padx=(20,20), pady=10)
        calculate_label.bind("<Button-1>", lambda event: self.openCalculateMortgagePage())

        status = ['New', 'Sold']
        self.selected_status = tk.StringVar()
        self.selected_status.set("Status")

        # Search Listing
        self.search_entry = tk.Entry(menu_frame, bg='white', fg='black', font=("", 14))
        self.search_entry.insert(0, "Search Listing")
        self.search_entry.bind("<FocusIn>", lambda e: self.search_entry.delete(0, "end"))
        self.search_entry.pack(side='right', pady=10)
        self.search_entry.bind("<Return>", self.searchPropertyListing)

        dropdown_status = tk.OptionMenu(menu_frame, self.selected_status, *status)
        dropdown_status.config(font=("", 13), justify='left')
        dropdown_status.pack(side='right', pady=10, padx =10)

        # separator between home and average frames
        separator2 = ttk.Separator(menu_frame, orient='horizontal')
        separator2.place(relx=0, rely=0.98, relwidth=1, relheight=0.02)

        # listing Main Frame
        self.listing_frame = Frame(self.home_frame, bg='lightyellow')
        self.listing_frame.place(relx=0, rely=0.175, relwidth=1, relheight=0.825)

        self.viewAllNewPropertyListings()

    def viewAllNewPropertyListings(self):
        listings = controller.BuyerViewAllPropertyListingsController()
        self.allNewListings = listings.viewAllListings('Available')
        self.showListingPages(0, 'New', self.allNewListings, self.allNewListings[0:12])

    def viewAllSoldPropertyListings(self):
        listings = controller.BuyerViewAllPropertyListingsController()
        self.allSoldListings = listings.viewAllListings('Sold')
        self.showListingPages(0, 'Sold', self.allSoldListings, self.allSoldListings[0:12])

    def viewSavedPropertyListings(self):
        listings = controller.BuyerViewSavedPropertyListingsController()
        self.SavedListings = listings.viewSavedListings(self.buyer.user_id)
        self.showListingPages(0, 'Saved', self.SavedListings, self.SavedListings[0:12])

    def searchPropertyListing(self, event):
        status = self.selected_status.get()
        if status == 'New':
            self.searchNewPropertyListing()
        elif status == 'Sold':
            self.searchSoldPropertyListing()
        else:
            messagebox.showerror("Failed", "Choose 'New' or 'Sold'!")

    def searchNewPropertyListing(self):

        Searchlisting = self.search_entry.get()

        if (Searchlisting == ""):
            messagebox.showerror("Failed", "Please enter a name to search")
            return self.viewAllNewPropertyListings()

        listings = controller.BuyerSearchPropertyListingController()
        self.Searchedlistings = listings.searchPropertyListing(Searchlisting,'Available')

        self.showListingPages(0, 'New', self.Searchedlistings, self.Searchedlistings[0:12])

    def searchSoldPropertyListing(self):

        Searchlisting = self.search_entry.get()

        if (Searchlisting == ""):
            messagebox.showerror("Failed", "Please enter a name to search")
            return self.viewAllSoldPropertyListings()

        listings = controller.BuyerSearchPropertyListingController()
        self.Searchedlistings = listings.searchPropertyListing(Searchlisting,'Sold')

        self.showListingPages(0, 'Sold', self.Searchedlistings, self.Searchedlistings[0:12])

    def showListingPages(self, page_index, heading, all_listing, show_listing):

        for widget in self.listing_frame.winfo_children():
            widget.destroy()

        # label frame for public listing 
        label_frame = Frame(self.listing_frame, bg='lightyellow')
        label_frame.place(relx=0, rely=0, relwidth=1, relheight=0.075)

        # public_listing label 
        label = tk.Label(label_frame, text=f"{heading} Listing", bg='lightyellow', fg='black', font=("", 14, "bold"))
        label.pack(side='bottom',pady=5)

        # content frame for public listing 
        content_frame = Frame(self.listing_frame, bg='lightyellow')
        content_frame.place(relx=0, rely=0.075, relwidth=1, relheight=0.925)

        current_page = page_index

        if len(all_listing) == 0:
            page_number = 1
        elif len(all_listing) % 12 == 0:
            page_number = int(len(all_listing)/12)  
        else:
            page_number = int(len(all_listing)/12) + 1
        

        image_frame1 = Frame(content_frame, bg='lightyellow')
        image_frame1.place(relx=0, rely=0, relwidth=1, relheight=0.4)
        image_frame1.rowconfigure((0), weight=1, uniform='a')
        image_frame1.columnconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        title_frame1 = Frame(content_frame, bg='lightyellow')
        title_frame1.place(relx=0, rely=0.4, relwidth=1, relheight=0.05)
        title_frame1.rowconfigure((0), weight=1, uniform='a')
        title_frame1.columnconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        image_frame2 = Frame(content_frame, bg='lightyellow')
        image_frame2.place(relx=0, rely=0.45, relwidth=1, relheight=0.4)
        image_frame2.rowconfigure((0), weight=1, uniform='a')
        image_frame2.columnconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        title_frame2 = Frame(content_frame, bg='lightyellow')
        title_frame2.place(relx=0, rely=0.85, relwidth=1, relheight=0.05)
        title_frame2.rowconfigure((0), weight=1, uniform='a')
        title_frame2.columnconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        btn_frame = Frame(content_frame, bg='lightyellow')
        btn_frame.place(relx=0, rely=0.9, relwidth=1, relheight=0.1)
        btn_frame.rowconfigure((0), weight=1, uniform='a')
        btn_frame.columnconfigure((0,1,2,3,4,5,6,7,8), weight=1, uniform='a')

        # number label
        number_label = tk.Label(btn_frame, text=f"{current_page+1}/{page_number}", bg='lightyellow', fg='black', font=("", 13))
        number_label.grid(row=0, column=4, sticky='nwse')

        # previous label
        previous_label = tk.Label(btn_frame, text="<<", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        previous_label.grid(row=0, column=4, sticky='w')
        previous_label.bind("<Button-1>", lambda event, page_index=current_page-1: self.showListingPages(page_index, heading, all_listing, all_listing[page_index*12:(page_index*12)+12]) if page_index >= 0 else None)

        # previous label
        next_label = tk.Label(btn_frame, text=">>", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        next_label.grid(row=0, column=4, sticky='e')
        next_label.bind("<Button-1>", lambda event, page_index=current_page+1: self.showListingPages(page_index, heading, all_listing, all_listing[page_index*12:(page_index*12)+12]) if page_index < page_number else None)

        for i,listing in enumerate(show_listing):

            if i < 6:
                frame1 = Frame(image_frame1, bg='lightyellow')
                frame1.grid(row=0, column=i, sticky='nwse')

                img = Image.open(listing.img)
                img_resized = img.resize((150,150), Image.LANCZOS)
                tk_image = ImageTk.PhotoImage(img_resized)
                image_label = tk.Label(frame1, image=tk_image, bg='lightyellow', cursor='hand2', highlightthickness=1, highlightbackground='black')
                image_label.image = tk_image  # Store reference to image to prevent garbage collection
                image_label.pack(side='top', expand=True)
                image_label.bind("<Button-1>", lambda event, selectedListing = listing.listing_id: self.openViewListingInfo(selectedListing))

                label1 = Label(title_frame1, text=listing.title, bg='lightyellow', fg='black', font=("", 13))
                label1.grid(row=0, column=i, sticky='n')
                #label1.pack(side='top', expand=True, pady=20)

            else:
                frame1 = Frame(image_frame2, bg='lightyellow')
                frame1.grid(row=0, column=i%6, sticky='nwse')

                img = Image.open(listing.img)
                img_resized = img.resize((150,150), Image.LANCZOS)
                tk_image = ImageTk.PhotoImage(img_resized)
                image_label = tk.Label(frame1, image=tk_image, bg='lightyellow', cursor='hand2', highlightthickness=1, highlightbackground='black')
                image_label.image = tk_image  # Store reference to image to prevent garbage collection
                image_label.pack(side='top', expand=True)
                image_label.bind("<Button-1>", lambda event, selectedListing = listing.listing_id: self.openViewListingInfo(selectedListing))

                label1 = Label(title_frame2, text=listing.title, bg='lightyellow', fg='black', font=("", 13))
                label1.grid(row=0, column=i%6, sticky='n')
                #label1.pack(side='top', expand=True, pady=20)

    def openViewListingInfo(self, selectedListing): 
        self.home_frame.pack_forget()
        BuyerViewListingInfoPage(self.root,self.buyer,selectedListing)

    def openCalculateMortgagePage(self):
        CalculateMortgagePaymentPage(self.root,self.buyer)

    def openCheckAccountPage(self):
        self.home_frame.pack_forget()
        UserViewMyAccountPage(self.root, self.buyer.user_id, self.pagename)

    def logout(self):
        UserLogoutPage(self.root, self.home_frame)

class BuyerViewListingInfoPage:
    def __init__(self, root, buyer, listing_id):
        self.root = root
        self.buyer = buyer
        self.listing_id = listing_id
        self.viewListingInfo()
    
    def viewListingInfo(self):
        viewBuyerPropertyListingController =  controller.BuyerViewListingInfoController()
        self.propertyListing = viewBuyerPropertyListingController.viewListingInfo(self.listing_id, self.buyer.user_id)
        self.displayBuyerViewListingInfoPage()
    
    def shortlistPropertyListing(self):

        shortlist_controller = controller.BuyerShortlistPropertyListingController()   
        success = shortlist_controller.shortlistProperty(self.buyer.user_id, self.listing_id)
        
        if success:
            messagebox.showinfo("Shortlist Success", "Property Listing is saved successfully.")

        self.home_frame.pack_forget()
        self.viewListingInfo()

    def unshortlistPropertyListing(self):
        unshortlist_controller = controller.BuyerUnshortlistPropertyListingController()   
        success = unshortlist_controller.unshortlistProperty(self.buyer.user_id, self.listing_id)
        if success:
            messagebox.showinfo("Unshortlist Success", "Property Listing is unsaved successfully.")
        
        self.home_frame.pack_forget()
        self.viewListingInfo()

    def displayBuyerViewListingInfoPage(self):
         # Home Frame 
        
        #base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)

        # Title Label
        label_title = tk.Label(header_frame, text="View Listing", bg="lightblue",  fg='black', font=("", 22, "bold"))
        label_title.pack(expand=True, anchor=tk.CENTER)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # 2nd Frame
        content_frame = tk.Frame(self.home_frame, bg="white")
        content_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.9)# Allow content frame to expand

        #Image
        # Load and resize the image
        image_path = self.propertyListing.img
        image = Image.open(image_path)  # Replace with the actual image path
        image = image.resize((200, 200)) 
        photo = ImageTk.PhotoImage(image)

        # Create a label to display the image
        self.image_label = Label(content_frame, image=photo, bg='white', cursor='hand2', highlightthickness=1, highlightbackground='black')
        self.image_label.image = photo 
        self.image_label.place(relx=0.2, rely=0.05, relwidth= 0.16, relheight=0.32)

        # Title Label
        label_Title = Label(content_frame, text="Title" , bg="white", padx=10, pady=5, font=("", 15))
        label_Title.place(relx=0.45, rely=0.1, anchor=W)

        # Title Label
        title = Label(content_frame, text=self.propertyListing.title , bg="white", font=("", 15))
        title.place(relx=0.55, rely=0.1, anchor=W)

        # Status Label
        label_Status = Label(content_frame, text="Status", bg="white", padx=10, pady=5, font=("", 15))
        label_Status.place(relx=0.45, rely=0.175, anchor=W)

        status = Label(content_frame, text=self.propertyListing.status, bg="white", font=("", 15))
        status.place(relx=0.55, rely=0.175, anchor=W)

        # Seller Label
        label_Agent = Label(content_frame, text="Agent", bg="white", padx=10, pady=5, font=("", 15))
        label_Agent.place(relx=0.45, rely=0.25, anchor=tk.W)

        Agent = Label(content_frame, text=self.propertyListing.agent_name, bg="white", font=("", 15))
        Agent.place(relx=0.55, rely=0.25, anchor=tk.W)

        # Price Label
        label_price = Label(content_frame, text="Price", bg="white", padx=10, pady=5, font=("", 15))
        label_price.place(relx=0.45, rely=0.325, anchor=tk.W)

        # Price
        price = Label(content_frame, text=self.propertyListing.price, bg="white", font=("", 15))
        price.place(relx=0.55, rely=0.325, anchor=tk.W)

        label_description = Label(content_frame, text="Description: ", bg="white", font=("", 15))
        label_description.place(relx=0.2, rely=0.425, anchor=W)

        #Text box
        large_textbox = Text(content_frame, bg="white", borderwidth=0, relief="solid", font=("", 13))
        large_textbox.insert(END, self.propertyListing.description)
        large_textbox.configure(state='disabled')
        large_textbox.place(relx=0.2, rely=0.45, anchor=NW, relwidth=0.6, relheight=0.3)

        #View Count
        label_view = Label(content_frame, text="View Count: " + str(self.propertyListing.view_count), bg="white", padx=10, pady=5, font=("", 15))
        label_view.place(relx=0.2, rely=0.8, anchor=tk.W)
            
        label_shortlist = Label(content_frame, text="Shorlist Count: " + str(self.propertyListing.shortlist_count), bg="white", padx=10, pady=5, font=("", 15))
        label_shortlist.place(relx=0.8, rely=0.8, anchor=tk.E)


        cancel_button = tk.Button(content_frame,  text="Cancel",   bg="white", padx=10, pady=5, font=("", 12), command=self.openBuyerHomePage)
        cancel_button.place(relx=0.3, rely=0.9, anchor=tk.CENTER, width=100)

        if (self.propertyListing.shortlist == 1):
            unsave_button = tk.Button(content_frame,  text="Unsave",   bg="white", padx=10, pady=5, font=("", 12), command=self.unshortlistPropertyListing)#ShortListFunction
            unsave_button.place(relx=0.5, rely=0.9, anchor=tk.CENTER, width=100)            
        else: 
            save_button = tk.Button(content_frame,  text="Save",   bg="white", padx=10, pady=5, font=("", 12), command=self.shortlistPropertyListing)#ShortListFunction
            save_button.place(relx=0.5, rely=0.9, anchor=tk.CENTER, width=100)

        contact_button = tk.Button(content_frame,  text="Contact",  bg="white", padx=10, pady=5, font=("", 12), command=self.openViewAgentInfoPage) #Redirect to contact agent
        contact_button.place(relx=0.7, rely=0.9, anchor=tk.CENTER, width=100)

    def openBuyerHomePage(self):
        self.home_frame.pack_forget()
        BuyerHomePage(self.root, self.buyer)

    def openViewAgentInfoPage(self):
        ViewAgentInfoPage(None, self.propertyListing.agent_id, self.buyer)

class CalculateMortgagePaymentPage:
    def __init__(self, root, buyer):
        self.root = root
        self.buyer = buyer

        self.root1 = tk.Toplevel(self.root)
        self.root1.geometry("450x350")
        self.root1.title("Calculate")
        self.root1.minsize(450,350)
        self.root1.maxsize(450,350)
        self.displayCalculateMortgagePaymentPage()

    def displayCalculateMortgagePaymentPage(self):

        self.calculate_frame = Frame(self.root1, bg='lightyellow')
        self.calculate_frame.pack(fill='both', expand='yes')

        self.calculate_frame.columnconfigure((0,1),  weight=1, uniform='a')
        self.calculate_frame.rowconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        # Header LAbel
        header_label = tk.Label(self.calculate_frame, text="Calculate Your Morgtage Payment", font=("", 15, 'bold'), bg='lightblue')
        header_label.grid(row=0, column=0, sticky='nwse', columnspan=2)


        # Loan Amount
        loan_amount_label = tk.Label(self.calculate_frame, text="Loan Amount:", font=("", 13), bg='lightyellow')
        loan_amount_label.grid(row=1, column=0, sticky='w', padx=(30,0))

        loan_s_dollar_label = tk.Label(self.calculate_frame, text="S$", font=("", 13), bg='lightyellow')
        loan_s_dollar_label.grid(row=1, column=0, sticky='e')

        self.loan_amount_entry = tk.Entry(self.calculate_frame, width=20, font=("", 13))
        self.loan_amount_entry.grid(row=1, column=1, sticky='w')

        # Interest Rate
        interest_rate_label = tk.Label(self.calculate_frame, text="Interest Rate:", font=("", 13), bg='lightyellow')
        interest_rate_label.grid(row=2, column=0, sticky='w', padx=(30,0))

        percent_label = tk.Label(self.calculate_frame, text="%", font=("", 13), bg='lightyellow')
        percent_label.grid(row=2, column=0, sticky='e')

        self.interest_rate_entry = tk.Entry(self.calculate_frame, width=20, font=("", 13))
        self.interest_rate_entry.grid(row=2, column=1, sticky='w')

        # Loan Tenure
        loan_tenure_label = tk.Label(self.calculate_frame, text="Loan Tenure:", font=("", 13), bg='lightyellow')
        loan_tenure_label.grid(row=3, column=0, sticky='w', padx=(30,0))

        yrs_label = tk.Label(self.calculate_frame, text="Yrs", font=("", 12), bg='lightyellow')
        yrs_label.grid(row=3, column=0, sticky='e')

        self.loan_tenure_entry = tk.Entry(self.calculate_frame, width=20, font=("", 13))
        self.loan_tenure_entry.grid(row=3, column=1, sticky='w')

        # Calculate Button
        calculate_button = tk.Button(self.calculate_frame, text="Calculate", font=("", 13), command=self.calculateMortgage)
        calculate_button.grid(row=4, column=0, sticky='nwse', columnspan=2, padx=150, pady=10)

    def calculateMortgage(self):
        try:
            loan_amount = float(self.loan_amount_entry.get())
            annual_interest_rate = float(self.interest_rate_entry.get())
            years = int(self.loan_tenure_entry.get())
        except ValueError:
            messagebox.showerror("Error", "Please enter a valid number in all fields!")
            return 

        monthly_interest_rate = annual_interest_rate / 12 / 100
        months = years * 12
        monthly_payment = loan_amount * monthly_interest_rate / (1 - (1 + monthly_interest_rate) ** -months)
        monthly_payment = round(monthly_payment, 2)

        result = tk.Label(self.calculate_frame, text=f"Your Mortgage Payment Plan: S${monthly_payment}/month", font=("", 12, 'bold'), bg='lightyellow')
        result.grid(row=5, column=0, sticky='nwse', columnspan=2, padx=(10,10))

class SellerHomePage:
    def __init__(self, root, seller):
        self.root = root
        self.seller = seller
        self.pagename = 'seller'
        self.displaySellerHomePage()
        
    def displaySellerHomePage(self):

        # base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)

        # Agent ID label 
        agent_label = tk.Label(header_frame, text=self.seller.userName, bg='lightblue', fg='black', font=("", 18, "bold"))
        agent_label.pack(side='left', padx=20, pady=10)

        # Logout Button
        logout_btn = Button(header_frame, text=' Logout ', bg='lightyellow', font=("", 14, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.logout)
        logout_btn.pack(side='right', padx=20, pady=10)

        # Profile Button
        profile_btn = Button(header_frame, text=' Profile ', bg='lightyellow', font=("", 14, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.openCheckAccountPage)
        profile_btn.pack(side='right', pady=10)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # menu frame
        menu_frame = Frame(self.home_frame, bg='white')
        menu_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.075)

        # new label
        all_listings_label = tk.Label(menu_frame, text="All Listings", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        all_listings_label.pack(side='left', padx=(20,20), pady=10)
        all_listings_label.bind("<Button-1>", lambda event: self.viewAllPropertyListings())

        # line seprator
        line = tk.Frame(menu_frame, bg='black', width=2, height=40)
        line.pack(side='left', pady=10)

        # sold label
        my_listing_label = tk.Label(menu_frame, text="My Listings", bg='white', fg='black', cursor='hand2', font=("", 13, "bold"), justify = 'center')
        my_listing_label.pack(side='left', padx=(20,20), pady=10)
        my_listing_label.bind("<Button-1>", lambda event: self.viewMyPropertyListings())

        # Search Listing
        self.search_entry = tk.Entry(menu_frame, bg='white', fg='black', font=("", 14))
        self.search_entry.insert(0, "Search Listing")
        self.search_entry.bind("<FocusIn>", lambda e: self.search_entry.delete(0, "end"))
        self.search_entry.pack(side='right', pady=10, padx=20)
        self.search_entry.bind("<Return>", self.searchPropertyListing)

        # separator between home and average frames
        separator2 = ttk.Separator(menu_frame, orient='horizontal')
        separator2.place(relx=0, rely=0.98, relwidth=1, relheight=0.02)

        # listing Main Frame
        self.listing_frame = Frame(self.home_frame, bg='lightyellow')
        self.listing_frame.place(relx=0, rely=0.175, relwidth=1, relheight=0.825)

        self.viewAllPropertyListings()

    def viewAllPropertyListings(self):
        listings = controller.SellerViewAllPropertyListingsController()
        self.allListings = listings.viewAllPropertyListings()        
        self.showListingPages(0, 'All', self.allListings, self.allListings[0:12])
    
    def viewMyPropertyListings(self):
        listings = controller.SellerViewMyPropertyListingsController()
        self.allListings = listings.viewMyPropertyListings(self.seller.user_id)        
        self.showListingPages(0, 'My', self.allListings, self.allListings[0:12])

    def searchPropertyListing(self, event):

        title = self.search_entry.get()

        if (title == ""):
            messagebox.showerror("Failed", "Please enter a name to search")
            return self.viewAllPropertyListings()

        listings = controller.SearchPropertyListingController()
        self.Searchedlistings = listings.searchPropertyListing(title)
        self.showListingPages(0, 'Searched', self.Searchedlistings, self.Searchedlistings[0:12])

    def showListingPages(self, page_index, heading, all_listing, show_listing):

        for widget in self.listing_frame.winfo_children():
            widget.destroy()

        # label frame for public listing 
        label_frame = Frame(self.listing_frame, bg='lightyellow')
        label_frame.place(relx=0, rely=0, relwidth=1, relheight=0.075)

        # public_listing label 
        label = tk.Label(label_frame, text=f"{heading} Listing", bg='lightyellow', fg='black', font=("", 14, "bold"))
        label.pack(side='bottom',pady=5)

        # content frame for public listing 
        content_frame = Frame(self.listing_frame, bg='lightyellow')
        content_frame.place(relx=0, rely=0.075, relwidth=1, relheight=0.925)

        current_page = page_index

        if len(all_listing) == 0:
            page_number = 1
        elif len(all_listing) % 12 == 0:
            page_number = int(len(all_listing)/12)  
        else:
            page_number = int(len(all_listing)/12) + 1
        

        image_frame1 = Frame(content_frame, bg='lightyellow')
        image_frame1.place(relx=0, rely=0, relwidth=1, relheight=0.4)
        image_frame1.rowconfigure((0), weight=1, uniform='a')
        image_frame1.columnconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        title_frame1 = Frame(content_frame, bg='lightyellow')
        title_frame1.place(relx=0, rely=0.4, relwidth=1, relheight=0.05)
        title_frame1.rowconfigure((0), weight=1, uniform='a')
        title_frame1.columnconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        image_frame2 = Frame(content_frame, bg='lightyellow')
        image_frame2.place(relx=0, rely=0.45, relwidth=1, relheight=0.4)
        image_frame2.rowconfigure((0), weight=1, uniform='a')
        image_frame2.columnconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        title_frame2 = Frame(content_frame, bg='lightyellow')
        title_frame2.place(relx=0, rely=0.85, relwidth=1, relheight=0.05)
        title_frame2.rowconfigure((0), weight=1, uniform='a')
        title_frame2.columnconfigure((0,1,2,3,4,5), weight=1, uniform='a')

        btn_frame = Frame(content_frame, bg='lightyellow')
        btn_frame.place(relx=0, rely=0.9, relwidth=1, relheight=0.1)
        btn_frame.rowconfigure((0), weight=1, uniform='a')
        btn_frame.columnconfigure((0,1,2,3,4,5,6,7,8), weight=1, uniform='a')

        # number label
        number_label = tk.Label(btn_frame, text=f"{current_page+1}/{page_number}", bg='lightyellow', fg='black', font=("", 13))
        number_label.grid(row=0, column=4, sticky='nwse')

        # previous label
        previous_label = tk.Label(btn_frame, text="<<", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        previous_label.grid(row=0, column=4, sticky='w')
        previous_label.bind("<Button-1>", lambda event, page_index=current_page-1: self.showListingPages(page_index, heading, all_listing, all_listing[page_index*12:(page_index*12)+12]) if page_index >= 0 else None)

        # previous label
        next_label = tk.Label(btn_frame, text=">>", bg='lightyellow', cursor='hand2', fg='black', font=("", 13))
        next_label.grid(row=0, column=4, sticky='e')
        next_label.bind("<Button-1>", lambda event, page_index=current_page+1: self.showListingPages(page_index,  heading, all_listing, all_listing[page_index*12:(page_index*12)+12]) if page_index < page_number else None)

        for i,listing in enumerate(show_listing):

            if i < 6:
                frame1 = Frame(image_frame1, bg='lightyellow')
                frame1.grid(row=0, column=i, sticky='nwse')

                img = Image.open(listing.img)
                img_resized = img.resize((150,150), Image.LANCZOS)
                tk_image = ImageTk.PhotoImage(img_resized)
                image_label = tk.Label(frame1, image=tk_image, bg='lightyellow', cursor='hand2', highlightthickness=1, highlightbackground='black')
                image_label.image = tk_image  # Store reference to image to prevent garbage collection
                image_label.pack(side='top', expand=True)
                image_label.bind("<Button-1>", lambda event, selectedListing = listing.listing_id: self.openViewListingInfo(selectedListing))

                label1 = Label(title_frame1, text=listing.title, bg='lightyellow', fg='black', font=("", 13))
                label1.grid(row=0, column=i, sticky='n')

            else:
                frame1 = Frame(image_frame2, bg='lightyellow')
                frame1.grid(row=0, column=i%6, sticky='nwse')

                img = Image.open(listing.img)
                img_resized = img.resize((150,150), Image.LANCZOS)
                tk_image = ImageTk.PhotoImage(img_resized)
                image_label = tk.Label(frame1, image=tk_image, bg='lightyellow', cursor='hand2', highlightthickness=1, highlightbackground='black')
                image_label.image = tk_image  # Store reference to image to prevent garbage collection
                image_label.pack(side='top', expand=True)
                image_label.bind("<Button-1>", lambda event, selectedListing = listing.listing_id: self.openViewListingInfo(selectedListing))

                label1 = Label(title_frame2, text=listing.title, bg='lightyellow', fg='black', font=("", 13))
                label1.grid(row=0, column=i%6, sticky='n')

    def openViewListingInfo(self, selectedListing): 
        self.home_frame.pack_forget()
        SellerViewListingInfoPage(self.root,self.seller,selectedListing)

    def openCheckAccountPage(self):
        self.home_frame.pack_forget()
        UserViewMyAccountPage(self.root, self.seller.user_id, self.pagename)

    def logout(self):
        UserLogoutPage(self.root, self.home_frame)

class SellerViewListingInfoPage:
    def __init__(self, root, seller, listing_id):
        self.root = root
        self.seller = seller
        self.listing_id = listing_id
        self.viewListingInfo()

    def viewListingInfo(self):

        viewPropertyListingController = controller.SellerViewListingInfoController()
        self.propertyListing = viewPropertyListingController.viewListingInfo(self.listing_id)
 
        self.displaySellerViewListingInfoPage()

    def displaySellerViewListingInfoPage(self):

        #base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)

        # Title Label
        label_title = tk.Label(header_frame, text="View Listing", bg="lightblue",  fg='black', font=("", 22, "bold"))
        label_title.pack(expand=True, anchor=tk.CENTER)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # 2nd Frame
        content_frame = tk.Frame(self.home_frame, bg="white")
        content_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.9)# Allow content frame to expand

        #Image
        # Load and resize the image
        image_path = self.propertyListing.img
        image = Image.open(image_path)  # Replace with the actual image path
        image = image.resize((200, 200)) 
        photo = ImageTk.PhotoImage(image)

        # Create a label to display the image
        self.image_label = Label(content_frame, image=photo, bg='white', cursor='hand2', highlightthickness=1, highlightbackground='black')
        self.image_label.image = photo 
        self.image_label.place(relx=0.2, rely=0.05, relwidth= 0.16, relheight=0.32)

        # Title Label
        label_Title = Label(content_frame, text="Title" , bg="white", padx=10, pady=5, font=("", 15))
        label_Title.place(relx=0.45, rely=0.1, anchor=W)

        # Title Label
        title = Label(content_frame, text=self.propertyListing.title , bg="white", font=("", 15))
        title.place(relx=0.55, rely=0.1, anchor=W)

        # Status Label
        label_Status = Label(content_frame, text="Status", bg="white", padx=10, pady=5, font=("", 15))
        label_Status.place(relx=0.45, rely=0.175, anchor=W)

        status = Label(content_frame, text=self.propertyListing.status, bg="white", font=("", 15))
        status.place(relx=0.55, rely=0.175, anchor=W)

        # Seller Label
        label_Agent = Label(content_frame, text="Agent", bg="white", padx=10, pady=5, font=("", 15))
        label_Agent.place(relx=0.45, rely=0.25, anchor=tk.W)

        Agent = Label(content_frame, text=self.propertyListing.agent_name, bg="white", font=("", 15))
        Agent.place(relx=0.55, rely=0.25, anchor=tk.W)

        # Price Label
        label_price = Label(content_frame, text="Price", bg="white", padx=10, pady=5, font=("", 15))
        label_price.place(relx=0.45, rely=0.325, anchor=tk.W)

        # Price
        price = Label(content_frame, text=self.propertyListing.price, bg="white", font=("", 15))
        price.place(relx=0.55, rely=0.325, anchor=tk.W)

        label_description = Label(content_frame, text="Description: ", bg="white", font=("", 15))
        label_description.place(relx=0.2, rely=0.425, anchor=W)

        #Text box
        large_textbox = Text(content_frame, bg="white", borderwidth=0, relief="solid", font=("", 13))
        large_textbox.insert(END, self.propertyListing.description)
        large_textbox.configure(state='disabled')
        large_textbox.place(relx=0.2, rely=0.45, anchor=NW, relwidth=0.6, relheight=0.3)

        #View Count
        label_view = Label(content_frame, text="View Count: " + str(self.propertyListing.view_count), bg="white", padx=10, pady=5, font=("", 15))
        label_view.place(relx=0.2, rely=0.8, anchor=tk.W)
            
        label_shortlist = Label(content_frame, text="Shorlist Count: " + str(self.propertyListing.shortlist_count), bg="white", padx=10, pady=5, font=("", 15))
        label_shortlist.place(relx=0.8, rely=0.8, anchor=tk.E)


        cancel_button = tk.Button(content_frame,  text="Cancel",   bg="white", font=("", 12), command=self.openSellerHomePage)
        cancel_button.place(relx=0.3, rely=0.9, anchor=tk.W, width=100)

        contact_button = tk.Button(content_frame,  text="Contact",  bg="white", font=("", 12), command=self.openContactPage) #Redirect to contact agent
        contact_button.place(relx=0.7, rely=0.9, anchor=tk.E, width=100)

    def openSellerHomePage(self):
        self.home_frame.pack_forget()
        SellerHomePage(self.root, self.seller)

    def openContactPage(self):
        ViewAgentInfoPage(None, self.propertyListing.agent_id, self.seller)

class ViewAgentInfoPage:
    def __init__(self, root, agent_id, customer):
        self.root = root
        self.agent_id = agent_id
        self.customer = customer

        if self.root == None:
            self.root =Tk()
            self.root.geometry("500x500")
            self.root.title("Agent Profile")
            self.root.minsize(500,500)
            self.root.maxsize(500,500)

        self.displayViewAgentInfoPage()

    def displayViewAgentInfoPage(self):
        # Home Frame 
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.15)

        label_title = tk.Label(header_frame, text="Agent Profile", bg="lightblue", font=("", 18, "bold"))
        label_title.pack(fill='both', expand=True)

        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        self.viewAgentInfo()

    def viewAgentInfo(self):
        
        viewAgentInfo =  controller.CustomerViewAgentInfoController()
        self.agentInfo = viewAgentInfo.viewAgentInfo(self.agent_id)
        self.displayAgentInfo()

    def displayAgentInfo(self):

        content_frame = tk.Frame(self.home_frame, bg="white")
        content_frame.place(relx=0, rely=0.15, relwidth=1, relheight=0.85)

        label_name = tk.Label(content_frame, text="Name", bg="white", padx=10, pady=5, font=("", 15))
        label_name.place(relx=0.2, rely=0.2, anchor=tk.W)

        agent_name = tk.Label(content_frame, text=self.agentInfo.userName, bg="white", padx=10, pady=5, font=("", 15))
        agent_name.place(relx=0.6, rely=0.2, anchor=tk.W)

        label_phone = tk.Label(content_frame, text="Phone Number", bg="white", padx=10, pady=5, font=("", 15))
        label_phone.place(relx=0.2, rely=0.4, anchor=tk.W)

        phone = tk.Label(content_frame, text=self.agentInfo.phoneNo, bg="white", padx=10, pady=5, font=("", 15))
        phone.place(relx=0.6, rely=0.4, anchor=tk.W)

        label_email = tk.Label(content_frame, text="Email", bg="white", padx=10, pady=5, font=("", 15))
        label_email.place(relx=0.2, rely=0.6, anchor=tk.W)

        email = tk.Label(content_frame, text=self.agentInfo.mail, bg="white", padx=10, pady=5, font=("", 15))
        email.place(relx=0.6, rely=0.6, anchor=tk.W)

        rate_button = tk.Button(content_frame,  text="Rate",  bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.openRateAgentPage)
        rate_button.place(relx=0.3, rely=0.8,  anchor=tk.CENTER, width=100)

        review_button = tk.Button(content_frame,  text="Review",  bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.openReviewAgentPage)
        review_button.place(relx=0.7, rely=0.8,  anchor=tk.CENTER, width=100)

    def openRateAgentPage(self):
        self.home_frame.pack_forget()
        CustomerRateAgentPage(self.root, self.agentInfo, self.customer)

    def openReviewAgentPage(self):
        self.home_frame.pack_forget()
        CustomerReviewAgentPage(self.root, self.agentInfo, self.customer)

class CustomerRateAgentPage:
    def __init__(self, root, agent, customer):
        self.root = root
        self.agent = agent
        self.customer = customer
        self.rating = None
        self.displayCustomerRateAgentPage()

    def displayCustomerRateAgentPage(self):
        # Home Frame 
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.15)

        label_title = tk.Label(header_frame, text="Rate the Agent", bg="lightblue", font=("", 18, "bold"))
        label_title.pack(fill='both', expand=True)

        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        content_frame = tk.Frame(self.home_frame, bg="white")
        content_frame.place(relx=0, rely=0.15, relwidth=1, relheight=0.85)

        check_options = ["Excellent", "Good", "Average", "Bad", "Worse"]
        self.check_var = tk.StringVar(content_frame)

        for option in check_options:
            check_button = tk.Checkbutton(content_frame, text=option, variable=self.check_var, onvalue=option, offvalue="", command=self.selection, bg="white", font=("", 14))
            check_button.pack(pady=10)

        # Cancel Button
        cancel_button = tk.Button(content_frame,  text="Cancel",  bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.openViewAgentInfoPage )
        cancel_button.place(relx=0.3, rely=0.8, anchor=tk.CENTER, width=100)

        # Rate Button
        rate_button = tk.Button(content_frame,  text="Rate",  bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.rateAgent)
        rate_button.place(relx=0.7, rely=0.8, anchor=tk.CENTER, width=100)

    def selection(self):
        self.rating = self.check_var.get()

    def rateAgent(self):

        while (True):
            if (self.rating):
                conversion = {"Worse": 1, "Bad": 2, "Average": 3, "Good": 4, "Excellent": 5}
                rating_num =  conversion.get(self.rating)
            else:
                messagebox.showinfo("Rating Failed", "Tick the checkbox to rate the Agent.")
                break

            rateAgent = controller.CustomerRateAgentController()
            result = rateAgent.rateAgent(self.customer.user_id, self.agent.user_id, rating_num)

            if (result):
                messagebox.showinfo("Rating Success", "You have successfully rated the Agent.")
                self.openViewAgentInfoPage()
            else:
                messagebox.showinfo("Rating Failed", "System Error!")
            break
        
    def openViewAgentInfoPage(self):
        self.home_frame.pack_forget()
        ViewAgentInfoPage(self.root, self.agent.user_id, self.customer)

class CustomerReviewAgentPage:
    def __init__(self, root, agent, customer):
        self.root = root
        self.agent = agent
        self.customer = customer
        self.displayCustomerReviewAgentPage()

    def displayCustomerReviewAgentPage(self):
        # Home Frame 
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.15)

        label_title = tk.Label(header_frame, text="Review the Agent", bg="lightblue", font=("", 18, "bold"))
        label_title.pack(fill='both', expand=True)

        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        content_frame = tk.Frame(self.home_frame, bg="white")
        content_frame.place(relx=0, rely=0.15, relwidth=1, relheight=0.85)

        label_description = Label(content_frame, text="Write a review about the agent", bg="white", pady=5, font=("", 15))
        label_description.place(relx=0.1, rely=0.1, anchor=W)
                
        #Text box
        self.large_textbox = Text(content_frame, width=45, height=12, borderwidth=1, relief="solid", font=("", 12))
        self.large_textbox.place(relx=0.1, rely=0.4, anchor=W)

        # Cancel Button
        cancel_button = tk.Button(content_frame,  text="Cancel",  bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.openViewAgentInfoPage)
        cancel_button.place(relx=0.3, rely=0.8, anchor=tk.CENTER, width=100)

        # Rate Button
        review_button = tk.Button(content_frame,  text="Review",  bg="lightgrey", padx=10, pady=5, font=("", 12), command=self.reviewAgent)
        review_button.place(relx=0.7, rely=0.8, anchor=tk.CENTER, width=100)

    def reviewAgent(self):
        
        review = self.large_textbox.get("1.0", "end-1c")

        while (True):
            if len(review) == 0:
                messagebox.showinfo("Review Failed", "Write the text to review for the Agent.")
                break

            reviewAgent = controller.CustomerReviewAgentController()
            result = reviewAgent.reviewAgent(self.customer.user_id, self.agent.user_id, review)

            if (result):
                messagebox.showinfo("Review Success", "You have successfully written a review for the Agent.")
                self.openViewAgentInfoPage()
            else:
                messagebox.showinfo("Review Failed", "System Error")
            break

    def openViewAgentInfoPage(self):
        self.home_frame.pack_forget()
        ViewAgentInfoPage(self.root, self.agent.user_id, self.customer)

class UserViewMyAccountPage:
    def __init__(self, root, user_id, page):
        self.root = root
        self.user_id = user_id
        self.page = page
        self.viewMyAccount()

    def viewMyAccount(self):

        self.viewAccountController = controller.UserViewMyAccountController()
        self.user = self.viewAccountController.viewMyAccount(self.user_id)
 
        self.displayUserViewMyAccountPage()

    def displayUserViewMyAccountPage(self):

        #base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)
        
        # Title Label
        label_title = tk.Label(header_frame, text="View Account", bg="lightblue",  fg='black', font=("", 22, "bold"))
        label_title.pack(expand=True, anchor=tk.CENTER)
    
        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)
        
        # 2nd Frame
        content_frame = tk.Frame(self.home_frame, bg="white")
        content_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.9)# Allow content frame to expand
        
        # userId Label
        label_userid = tk.Label(content_frame, text="User ID", bg="white", padx=10, pady=5, font=("", 15))
        label_userid.place(relx=0.35, rely=0.1, anchor=tk.W)
        
        # userId 
        userId = tk.Label(content_frame, text=self.user.user_id, bg="white", font=("", 15))
        userId.place(relx=0.55, rely=0.1, anchor=tk.W)

        # Name Label
        label_name = tk.Label(content_frame, text="Name", bg="white", padx=10, pady=5, font=("", 15))
        label_name.place(relx=0.35, rely=0.2, anchor=tk.W)

        # Name
        name = tk.Label(content_frame, text=self.user.userName, bg="white", font=("", 15))
        name.place(relx=0.55, rely=0.2, anchor=tk.W)
        
        # Phone Label
        label_phone = tk.Label(content_frame, text="Phone Number", bg="white", padx=10, pady=5, font=("", 15))
        label_phone.place(relx=0.35, rely=0.3, anchor=tk.W)
        
        # Phone
        phone = tk.Label(content_frame, text=self.user.phoneNo, bg="white", font=("", 15))
        phone.place(relx=0.55, rely=0.3, anchor=tk.W)

        # Email Label
        label_email = tk.Label(content_frame, text="Email", bg="white", padx=10, pady=5, font=("", 15))
        label_email.place(relx=0.35, rely=0.4, anchor=tk.W)

        # email
        email = tk.Label(content_frame, text=self.user.mail, bg="white", font=("", 15))
        email.place(relx=0.55, rely=0.4, anchor=tk.W)


        # status Label
        label_status = tk.Label(content_frame, text="Status", bg="white", padx=10, pady=5, font=("", 15))
        label_status.place(relx=0.35, rely=0.5, anchor=tk.W)
        
        if self.user.suspend == 1:
            active_status = 'Suspended'
        elif self.user.suspend == 0:
            active_status = 'Active'

        # status
        status = tk.Label(content_frame, text=active_status, bg="white", font=("", 15))
        status.place(relx=0.55, rely=0.5, anchor=tk.W)

        # Password Label
        label_password = tk.Label(content_frame, text="Password", bg="white", padx=10, pady=5, font=("", 15))
        label_password.place(relx=0.35, rely=0.6, anchor=tk.W)
        
        # password
        password = tk.Label(content_frame, text= "**********", bg="white", font=("", 15))
        password.place(relx=0.55, rely=0.6, anchor=tk.W)
        
        # role Label
        label_role = tk.Label(content_frame, text="Role", bg="white", padx=10, pady=5, font=("", 15))
        label_role.place(relx=0.35, rely=0.7, anchor=tk.W)

        # role 
        role = tk.Label(content_frame, text=self.user.profileName, bg="white", font=("", 15))
        role.place(relx=0.55, rely=0.7, anchor=tk.W)
        
        #Cancel Button
        cancel_button = tk.Button(content_frame,  text="Cancel",  bg="lightgray", padx=10, pady=5, font=("", 12), command=self.openHomePage)
        cancel_button.place(relx=0.47, rely=0.85, anchor=tk.E, width=100)
        
        change_pwd_button = tk.Button(content_frame,  text="Change Password",  bg="lightgray", padx=10, pady=5, font=("", 10), command=self.openUpdatePasswordPage)
        change_pwd_button.place(relx=0.53, rely=0.85, anchor=tk.W, width=120)

    def openHomePage(self):

        if self.user.profileName == "system admin":
            self.openAdminPage()
        elif self.user.profileName == "real estate agent":
            self.openAgentPage()
        elif self.user.profileName == "buyer":
            self.openBuyerPage()
        elif self.user.profileName == "seller":
            self.openSellerPage()
        else:
            self.openUnderDevelopmentPage()

    def openAdminPage(self):
        self.home_frame.pack_forget()
        if self.page == "ManageAccount" :
            AdminManageUserAccountsPage(self.root, self.user)
        else:
            AdminManageUserProfilesPage(self.root, self.user)  

    def openAgentPage(self):
        self.home_frame.pack_forget()
        AgentHomePage(self.root, self.user)

    def openBuyerPage(self):
        self.home_frame.pack_forget()
        BuyerHomePage(self.root, self.user)

    def openSellerPage(self):
        self.home_frame.pack_forget()
        SellerHomePage(self.root, self.user)

    def openUnderDevelopmentPage(self):
        self.home_frame.pack_forget()
        UnderDevelopmentPage(self.root, self.user)

    def openUpdatePasswordPage(self):
        UserUpdatePasswordPage(self.root, self.user, self.home_frame)

class UserUpdatePasswordPage:
    def __init__(self, root, user, passedFrame):
        self.root = root
        self.user = user
        self.passedFrame = passedFrame

        self.root1 = tk.Toplevel(self.root)
        self.root1.geometry("450x350")
        self.root1.title("Change Password")
        self.root1.minsize(450,350)
        self.root1.maxsize(450,350)
        self.displayUserUpdatePasswordPage()
    
    def displayUserUpdatePasswordPage(self):
        self.change_pwd_frame = Frame(self.root1, bg='lightyellow')
        self.change_pwd_frame.pack(fill='both', expand='yes')

        self.change_pwd_frame.columnconfigure((0,1),  weight=1, uniform='a')
        self.change_pwd_frame.rowconfigure((0,1,2,3,4), weight=1, uniform='a')

        # Header LAbel
        header_label = tk.Label(self.change_pwd_frame, text="Change Your Password", font=("", 15, 'bold'), bg='lightblue')
        header_label.grid(row=0, column=0, sticky='nwse', columnspan=2)

        # Loan Amount
        old_password_label = tk.Label(self.change_pwd_frame, text="Old Password:", font=("", 13), bg='lightyellow')
        old_password_label.grid(row=1, column=0, sticky='w', padx=(30,0))

        self.old_password_entry = tk.Entry(self.change_pwd_frame, width=20, font=("", 13), show='*')
        self.old_password_entry.grid(row=1, column=1, sticky='w')

        # show/hide password 
        self.show_image = ImageTk.PhotoImage(file='images/show.png')
        self.hide_image = ImageTk.PhotoImage(file='images/hide.png')


        self.show_button1 = tk.Button(self.change_pwd_frame, image=self.show_image, command=lambda: self.show(1), relief=FLAT,
                                  activebackground="lightyellow", borderwidth=0, background="lightyellow", cursor="hand2")
        self.show_button1.grid(row=1, column=1, sticky='e', padx=(0,20))
        
        # Interest Rate
        new_password_label = tk.Label(self.change_pwd_frame, text="New Password:", font=("", 13), bg='lightyellow')
        new_password_label.grid(row=2, column=0, sticky='w', padx=(30,0))

        self.new_password_entry = tk.Entry(self.change_pwd_frame, width=20, font=("", 13), show='*')
        self.new_password_entry.grid(row=2, column=1, sticky='w')

        self.show_button2 = tk.Button(self.change_pwd_frame, image=self.show_image, command=lambda: self.show(2), relief=FLAT,
                                  activebackground="lightyellow", borderwidth=0, background="lightyellow", cursor="hand2")
        self.show_button2.grid(row=2, column=1, sticky='e', padx=(0,20))

        # Loan Tenure
        retype_new_password_label = tk.Label(self.change_pwd_frame, text="Retype New Password:", font=("", 13), bg='lightyellow')
        retype_new_password_label.grid(row=3, column=0, sticky='w', padx=(30,0))

        self.retype_new_password_entry = tk.Entry(self.change_pwd_frame, width=20, font=("", 13), show='*')
        self.retype_new_password_entry.grid(row=3, column=1, sticky='w')

        self.show_button3 = tk.Button(self.change_pwd_frame, image=self.show_image, command=lambda: self.show(3), relief=FLAT,
                                  activebackground="lightyellow", borderwidth=0, background="lightyellow", cursor="hand2")
        self.show_button3.grid(row=3, column=1, sticky='e', padx=(0,20))


        # Calculate Button
        change_button = tk.Button(self.change_pwd_frame, text="Change Password", font=("", 13), command=self.updatePassword)
        change_button.grid(row=4, column=0, sticky='nwse', columnspan=2, padx=150, pady=20)

    def updatePassword(self):

        old_password = self.old_password_entry.get()
        new_password = self.new_password_entry.get()
        retype_new_password = self.retype_new_password_entry.get()

        passwordPattern = r"^(?=.*[a-zA-Z0-9])(?=.*[!@#$%^&*])(?=.*[a-z])(?=.*[A-Z]).{8,}$"

        while True:
            # Check if any field is empty
            if old_password == "" or new_password == "" or retype_new_password == "":
                messagebox.showerror("Failed", "Fill all fields to update the password")
                break

            elif self.hash_password(old_password) != self.user.pwd:
                messagebox.showerror("Failed", "Old password does not match")
                break

            elif re.match(passwordPattern,new_password) is None:
                messagebox.showerror("Failed","Password must be more than 8 characters and it must contain at least a lowercase, a uppercase, a special character and a number.")
                break
            
            elif new_password != retype_new_password:
                messagebox.showerror("Failed","Retyped password must match the new password")
                break

            elif new_password == old_password:
                messagebox.showerror("Failed", "Old password cannot be the same as new password")
                break

            else:
                hashed_Password = self.hash_password(new_password)
                result = controller.UserUpdatePasswordController().updatePassword(self.user.user_id, hashed_Password)

                if result:
                    messagebox.showinfo("Successful!", "Password updated successfully!")
                    self.root1.destroy()
                    self.passedFrame.pack_forget()
                    UserViewMyAccountPage(self.root, self.user.user_id, "")
                else:
                    messagebox.showerror("Failed!", "Failed to update password. Please check your old password.")
                break

    def hash_password(self, password):
        # Convert the password string to bytes
        password_bytes = password.encode('utf-8')

        # Create a SHA-256 hash object
        sha256_hash = hashlib.sha256()

        # Update the hash object with the password bytes
        sha256_hash.update(password_bytes)

        # Get the hexadecimal representation of the hash
        hashed_password = sha256_hash.hexdigest()

        return hashed_password

    def show(self, num):
        if num == 1:
            self.hide_button1 = Button(self.change_pwd_frame, image=self.hide_image, command=lambda: self.hide(1), relief=FLAT,
                                  activebackground="lightyellow", borderwidth=0, background="lightyellow", cursor="hand2")
            self.hide_button1.grid(row=1, column=1, sticky='e', padx=(0,20))
            self.old_password_entry.config(show='')
        elif num == 2:
            self.hide_button2 = Button(self.change_pwd_frame, image=self.hide_image, command=lambda: self.hide(2), relief=FLAT,
                                  activebackground="lightyellow", borderwidth=0, background="lightyellow", cursor="hand2")
            self.hide_button2.grid(row=2, column=1, sticky='e', padx=(0,20))
            self.new_password_entry.config(show='')
        elif num == 3:
            self.hide_button3 = Button(self.change_pwd_frame, image=self.hide_image, command=lambda: self.hide(3), relief=FLAT,
                                  activebackground="lightyellow", borderwidth=0, background="lightyellow", cursor="hand2")
            self.hide_button3.grid(row=3, column=1, sticky='e', padx=(0,20))
            self.retype_new_password_entry.config(show='')

    def hide(self,num):
        if num == 1:
            self.show_button1 = Button(self.change_pwd_frame, image=self.show_image, command=lambda: self.show(1), relief=FLAT,
                                  activebackground="lightyellow", borderwidth=0, background="lightyellow", cursor="hand2")
            self.show_button1.grid(row=1, column=1, sticky='e', padx=(0,20))
            self.old_password_entry.config(show='*')
        if num == 2:
            self.show_button2 = Button(self.change_pwd_frame, image=self.show_image, command=lambda: self.show(2), relief=FLAT,
                                  activebackground="lightyellow", borderwidth=0, background="lightyellow", cursor="hand2")
            self.show_button2.grid(row=2, column=1, sticky='e', padx=(0,20))
            self.new_password_entry.config(show='*')
        if num == 3:
            self.show_button3 = Button(self.change_pwd_frame, image=self.show_image, command=lambda: self.show(3), relief=FLAT,
                                  activebackground="lightyellow", borderwidth=0, background="lightyellow", cursor="hand2")
            self.show_button3.grid(row=3, column=1, sticky='e', padx=(0,20))
            self.retype_new_password_entry.config(show='*')

class UnderDevelopmentPage:
    def __init__(self, root, user):
        self.root = root
        self.user = user
        self.pagename = 'guest'
        self.displayUnderDevelopmentPage()
        
    def displayUnderDevelopmentPage(self):
        # base frame
        self.home_frame = Frame(self.root,  bg='white')
        self.home_frame.pack(fill='both', expand='yes')

        # header frame
        header_frame = Frame(self.home_frame, bg='lightblue')
        header_frame.place(relx=0, rely=0, relwidth=1, relheight=0.1)

        # Agent ID label 
        user_label = tk.Label(header_frame, text=self.user.userName, bg='lightblue', fg='black', font=("", 18, "bold"))
        user_label.pack(side='left', padx=20, pady=10)

        # Logout Button
        logout_btn = Button(header_frame, text=' Logout ', bg='lightyellow', font=("", 14, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.logout)
        logout_btn.pack(side='right', padx=20, pady=10)

        # Profile Button
        profile_btn = Button(header_frame, text=' Profile ', bg='lightyellow', font=("", 14, "bold"), bd=1, fg='black', cursor='hand2', activebackground='#32cf8e', command=self.openCheckAccountPage)
        profile_btn.pack(side='right', pady=10)

        # separator between home and average frames
        separator1 = ttk.Separator(header_frame, orient='horizontal')
        separator1.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # menu frame
        menu_frame = Frame(self.home_frame, bg='white')
        menu_frame.place(relx=0, rely=0.1, relwidth=1, relheight=0.075)

        # separator between home and average frames
        separator2 = ttk.Separator(menu_frame, orient='horizontal')
        separator2.place(relx=0, rely=0.99, relwidth=1, relheight=0.01)

        # Content Frame
        content_frame = Frame(self.home_frame, bg='lightyellow')
        content_frame.place(relx=0, rely=0.175, relwidth=1, relheight=0.825)

        # Content label 
        content_label = tk.Label(content_frame, text="Your Page is Under Development.\nContact Admin for more information", bg='lightyellow', fg='black', font=("", 18, "bold"))
        content_label.pack(fill="both", expand="True")

    def logout(self):
        UserLogoutPage(self.root, self.home_frame)

    def openCheckAccountPage(self):
        self.home_frame.pack_forget()
        UserViewMyAccountPage(self.root, self.user.user_id, self.pagename)

def page():
    window = Tk()
    window.geometry("1280x720")
    window.state('zoomed')
    window.title('Home Smith')
    window.minsize(960,540)
    UserLoginPage(window)
    window.mainloop()

if __name__ == '__main__':
    page()