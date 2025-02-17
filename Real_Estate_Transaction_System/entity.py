import mysql.connector

mydb = mysql.connector.connect(
    host = "localhost",
    user = "root",
    passwd = "",
    database = "real_estate"
)

class UserProfile:
    def __init__(self, profileName=None, description=None, suspend=None):
        self.profileName = profileName
        self.description = description
        self.suspend = suspend

    def createUserProfile(self, profileName,description,suspend):
        try:
            mycursor = mydb.cursor()
            query = "INSERT INTO user_profiles (profileName, description, suspend) VALUES (%s, %s, %s)"
            data = (profileName, description, suspend)
            mycursor.execute(query, data)
            rowsAffected = mycursor.rowcount

            mydb.commit()
            if (rowsAffected > 0):
                return True
            else:
                return False
        except Exception as e:
            print(f"Error creating profile: {e}")
            return False

    def viewAllUserProfiles(self):

        mycursor = mydb.cursor()
        query = "SELECT profileName, suspend FROM user_profiles"
        mycursor.execute(query)

        allProfiles = mycursor.fetchall()
        
        mydb.commit()
        profileList = []

        if allProfiles:
            for (profileName, suspend) in allProfiles:
                profile = UserProfile(profileName, None, suspend)
                profileList.append(profile)
        return profileList

    def viewProfileInfo(self, profileName):

        mycursor = mydb.cursor()
        query = "SELECT * FROM user_profiles WHERE profileName = %s"
        data = [profileName]
        mycursor.execute(query, data)

        userProfile = mycursor.fetchone()
        mydb.commit()

        return UserProfile(userProfile[0], userProfile[1], userProfile[2])

    def updateUserProfile(self, profileName, description):
        try:
            mycursor = mydb.cursor()

            query = "UPDATE user_profiles SET description = %s WHERE profileName = %s"
            data = (description, profileName)
            mycursor.execute(query, data)
            mydb.commit()

            return True
        
        except Exception as e:
            print(f"Error updating user profile: {e}")
            return False

    def suspendUserProfile(self, profileName):
        mycursor = mydb.cursor()
        query1 = "UPDATE user_profiles SET suspend = 1 WHERE profileName = %s" # 1 suspended 0 not suspended
        data = [profileName]
        mycursor.execute(query1, data)

        rowsAffected = mycursor.rowcount

        query2 = "UPDATE user_accounts SET suspend = 1 WHERE profileName = %s"
        data = [profileName]
        mycursor.execute(query2, data)

        mydb.commit()
        
        return True

    def unsuspendUserProfile(self, profileName):
        mycursor = mydb.cursor()
        query = "UPDATE user_profiles SET suspend = 0 WHERE profileName = %s" # 1 suspended 0 not suspended
        data = [profileName]
        mycursor.execute(query, data)

        query2 = "UPDATE user_accounts SET suspend = 0 WHERE profileName = %s"
        data = [profileName]
        mycursor.execute(query2, data)

        mydb.commit()

        return True

    def searchUserProfile(self, profileName):

        mycursor = mydb.cursor()
        query = "SELECT profileName, suspend FROM user_accounts WHERE profileName LIKE %s"

        data = (f'%{profileName}%',)
        mycursor.execute(query, data)
        allProfiles = mycursor.fetchall()

        mydb.commit()

        profileList = []

        if allProfiles:
            for ( profileName, suspend) in allProfiles:
                profile = UserProfile(profileName, None, suspend)
                profileList.append(profile)
        return profileList

    def fetchUserProfiles(self):

        mycursor = mydb.cursor()
        query = "SELECT profileName FROM user_profiles WHERE suspend = 0"
        mycursor.execute(query)
        
        allProfiles = mycursor.fetchall()
        mydb.commit()
        ProfileList = []

        for profile in allProfiles:
            ProfileList.append(profile)

        return ProfileList

class UserAccount:
    def __init__(self, user_id=None, userName=None, profileName=None, pwd=None, mail=None, phoneNo=None, suspend=None):
        self.user_id = user_id
        self.userName = userName
        self.profileName = profileName
        self.pwd = pwd
        self.mail =  mail
        self.phoneNo = phoneNo
        self.suspend = suspend
    
    def loginAccount(self, user_id, password):

        mycursor = mydb.cursor()
        query1 = "SELECT * FROM user_accounts WHERE user_id = %s AND pwd = %s"
        data = (user_id, password)
        mycursor.execute(query1, data)

        account = mycursor.fetchone()
        
        mydb.commit()

        if (account):
            return UserAccount(account[0], account[1], account[2], account[3], account[4], account[5], account[6])
        else:
            return None
                
    def createUserAccount(self, user_id, name, selectedProfile, password, email, phoneNum, suspend = 0):
        try:
            mycursor = mydb.cursor()
            query = "INSERT INTO user_accounts (user_id, name, profileName, pwd, mail, phoneNo, suspend) VALUES (%s, %s, %s, %s, %s, %s, %s)"
            data = (user_id,name,selectedProfile,password,email,phoneNum,suspend)
            mycursor.execute(query, data)
            rowsAffected = mycursor.rowcount
            mydb.commit()

            if (rowsAffected > 0):
                return True
            else:
                return False
        except Exception as e:
            print(f"Error creating account: {e}")
            return False
        
    def viewAllUserAccounts(self):

        mycursor = mydb.cursor()
        query = "SELECT user_id, profileName, suspend FROM user_accounts"
        mycursor.execute(query)

        allAccounts = mycursor.fetchall()
        mydb.commit()

        accountList = []

        if allAccounts:
            for (user_id, profileName, suspend) in allAccounts:
                account = UserAccount(user_id, None, profileName, None, None, None, suspend)
                accountList.append(account)
        return accountList

    def viewAccountInfo(self, user_id):

        mycursor = mydb.cursor()
        query = "SELECT * FROM user_accounts WHERE user_id = %s"
        data = [user_id]
        mycursor.execute(query, data)

        userAccount = mycursor.fetchone()
        mydb.commit()

        return UserAccount(userAccount[0], userAccount[1], userAccount[2], userAccount[3], userAccount[4], userAccount[5], userAccount[6])

    def updateUserAccount(self, user_id, name, password, email, phone):
        try:
            mycursor = mydb.cursor()
            query = "UPDATE user_accounts SET name = %s, pwd = %s, mail = %s, phoneNo = %s WHERE user_id = %s"
            data = (name, password, email, phone, user_id)
            mycursor.execute(query, data)
            
            mydb.commit()
            return True
        
        except Exception as e:
            print(f"Error creating account: {e}")
            return False

    def suspendUserAccount(self, user_id):
        mycursor = mydb.cursor()
        print("enter suspenduser")
        query = "UPDATE user_accounts SET suspend = 1 WHERE user_id = %s" # 1 suspended 0 not suspended
        data = [user_id]
        mycursor.execute(query, data)
        mydb.commit()

        return True
  
    def unsuspendUserAccount(self, user_id, profileName):
        mycursor = mydb.cursor()
        query1 = "SELECT suspend FROM user_profiles WHERE profileName = %s"
        data1 = [profileName]
        mycursor.execute(query1, data1)

        profileSuspend = mycursor.fetchone()
        
        query2 = "SELECT suspend FROM user_accounts WHERE user_id = %s"
        data2 = [user_id]
        mycursor.execute(query2, data2)

        accountSuspend = mycursor.fetchone()

        mydb.commit()

        if (profileSuspend == (0,)) and (accountSuspend == (1,)):
            query3 = "UPDATE user_accounts SET suspend = 0 WHERE user_id = %s" # 1 suspended 0 not suspended
            data3 = [user_id]
            mycursor.execute(query3, data3)
            return True
        else:
            return False

    def searchUserAccount(self, user_id):

        mycursor = mydb.cursor()
        query = "SELECT user_id, profileName, suspend FROM user_accounts WHERE user_id LIKE %s"

        data = (f'%{user_id}%',)
        mycursor.execute(query, data)
        allAccounts = mycursor.fetchall()
        mydb.commit()

        accountList = []

        if allAccounts:
            for (user_id, profileName, suspend) in allAccounts:
                account = UserAccount(user_id, None, profileName, None, None, None, suspend)
                accountList.append(account)
        return accountList
        
    def updatePassword(self, user_id, new_password):

        mycursor = mydb.cursor()

        query = "UPDATE user_accounts SET pwd = %s WHERE user_id = %s"
        data = (new_password, user_id)
        mycursor.execute(query, data)

        rowsAffected = mycursor.rowcount
        mydb.commit()

        if (rowsAffected > 0):
            return True
        else:
            return False

class PropertyListing:

    def __init__(self,listing_id=None, title=None, status=None, img=None, agent_id=None, seller_id=None, price=None, description=None, view_count=None, shortlist_count=None, suspend = 0, agent_name=None, shortlist = None):
        self.listing_id = listing_id
        self.title = title
        self.status = status
        self.img = img
        self.agent_id = agent_id
        self.seller_id = seller_id
        self.price = price
        self.description = description
        self.view_count = view_count
        self.shortlist_count = shortlist_count
        self.suspend = suspend
        self.agent_name = agent_name
        self.shortlist = shortlist

    def viewAllPropertyListings(self):

        mycursor = mydb.cursor()
        query = "SELECT listing_id, title, img FROM listings where suspend = 0"
        mycursor.execute(query)

        allListings = mycursor.fetchall()
        mydb.commit()
        listingList = []

        if allListings:
            for (listing_id, title, img) in allListings:
                listing = PropertyListing(listing_id, title, None, img, None, None, None, None, None, None, None, None, None)
                listingList.append(listing)
        return listingList
        
    def agentViewMyPropertyListings(self, agent_id):
        mycursor = mydb.cursor()
        query = "SELECT listing_id, title, img FROM listings where agent_id=%s"
        data =[agent_id]
        mycursor.execute(query,data)

        allListings = mycursor.fetchall()
        mydb.commit()
        listingList = []

        if allListings:
            for (listing_id, title, img) in allListings:
                listing = PropertyListing(listing_id, title, None, img, None, None, None, None, None, None, None, None, None)
                listingList.append(listing)
        return listingList

    def createPropertyListing(self, title, status, img, agent_id, seller_id, price, description, suspend = 0):
        try:
            mycursor = mydb.cursor()

            query1 =  "SELECT user_id FROM user_accounts WHERE profileName = 'seller'"
            mycursor.execute(query1)
            allSellerId = mycursor.fetchall()
            allSellerIds = [id[0] for id in allSellerId]

            if seller_id in allSellerIds:
                query2 = "INSERT INTO listings (title, status, img, agent_id, seller_id, price, description, suspend) VALUES (%s, %s, %s, %s, %s, %s, %s, %s)"
                data2 = (title, status, img, agent_id, seller_id, price, description, suspend)
                mycursor.execute(query2,data2)
                mydb.commit()
                return True
            else:
                mydb.commit()
                return False
        except Exception as e:
            print(f"Error creating listing: {e}")
            return False

    def viewListingInfo(self,listing_id):

        mycursor = mydb.cursor()
        query = "SELECT l.*, u.name FROM listings l Left JOIN user_accounts u ON l.agent_id = u.user_id WHERE l.listing_id = %s"
        data = [listing_id]
        mycursor.execute(query,data)
        selectedListing = mycursor.fetchone()

        mydb.commit()

        return PropertyListing(selectedListing[0], selectedListing[1], selectedListing[2], selectedListing[3], selectedListing[4], selectedListing[5], selectedListing[6], selectedListing[7], selectedListing[8], selectedListing[9],  selectedListing[10], selectedListing[11], None)    
               
    def updatePropertyListing(self, listing_id,  title, status, seller_id, price, description):
        try:
            mycursor = mydb.cursor()

            query1 =  "SELECT user_id FROM user_accounts WHERE profileName = 'seller'"
            mycursor.execute(query1)
            allSellerId = mycursor.fetchall()
            allSellerIds = [id[0] for id in allSellerId]

            if seller_id in allSellerIds:
                query2 = "UPDATE listings SET title = %s, status = %s,  seller_id = %s, price = %s, description = %s where listing_id = %s"
                data2 = (title, status, seller_id, price, description, listing_id)
                mycursor.execute(query2,data2)
                mydb.commit()
                return True
            else:
                mydb.commit()
                return False
        except Exception as e:
            print(f"Error updating listing: {e}")
            return False
    
    def suspendPropertyListing(self, listing_id):
        mycursor = mydb.cursor()
        query = "UPDATE listings SET suspend = 1 WHERE listing_id = %s" # 1 suspended 0 not suspended
        data = [listing_id]
        mycursor.execute(query, data)

        mydb.commit()

        return True

    def unsuspendPropertyListing(self, listing_id):
        mycursor = mydb.cursor()
        query = "UPDATE listings SET suspend = 0 WHERE listing_id = %s" # 1 suspended 0 not suspended
        data = [listing_id]
        mycursor.execute(query, data)

        mydb.commit()
        
        return True
    
    def searchPropertyListing(self, title):
        cursor = mydb.cursor()
        query = "SELECT listing_id, title, img FROM Listings WHERE title LIKE %s AND suspend = 0"
        data = (f"%{title}%",)
        cursor.execute(query,data)
        listings = cursor.fetchall()
        mydb.commit()
        listingList = []
        if listings:
            for (listing_id, title, img) in listings:
                listing = PropertyListing(listing_id, title, None, img, None,  None, None, None, None, None, None, None, None)
                listingList.append(listing)
        return listingList
      
    def buyerViewAllPropertyListings(self, status):
        mycursor = mydb.cursor()
        query = "SELECT listing_id, title, img FROM listings WHERE status = %s AND suspend = 0"
        data = [status]
        mycursor.execute(query, data)

        allListings = mycursor.fetchall()

        mydb.commit()
        listingList = []
        if allListings:
            for (listing_id, title, img) in allListings:
                listing = PropertyListing(listing_id, title, None, img, None, None, None, None, None, None, None, None, None)
                listingList.append(listing)
        return listingList

    def buyerViewSavedPropertyListings(self, buyer_id):
        mycursor = mydb.cursor()
        query = "SELECT l.listing_id, l.title, l.img FROM listings l Left JOIN shortlistings s ON l.listing_id = s.listing_id AND s.buyer_id = %s WHERE s.shortlist = 1 AND l.suspend=0"
        data = [buyer_id]
        mycursor.execute(query, data)

        allListings = mycursor.fetchall()

        mydb.commit()
        listingList = []

        if allListings:
            for (listing_id, title, img) in allListings:
                listing = PropertyListing(listing_id, title, None, img, None, None, None, None, None, None, None, None, None)
                listingList.append(listing)
        return listingList
        
    def buyerViewListingInfo(self,listing_id, buyer_id):
        mycursor = mydb.cursor()

        query1 = "UPDATE listings SET view_count = view_count + 1 where listing_id = %s"
        data1 = [listing_id]
        mycursor.execute(query1,data1)

        query2 = "SELECT l.*, u.name, s.shortlist FROM listings l Left JOIN shortlistings s ON l.listing_id = s.listing_id AND s.buyer_id = %s LEFT JOIN user_accounts u ON l.agent_id = u.user_id WHERE l.listing_id = %s"
        data2 = (buyer_id, listing_id)
        mycursor.execute(query2,data2)
        selectedListing = mycursor.fetchone()
        
        mydb.commit()

        return PropertyListing(selectedListing[0], selectedListing[1], selectedListing[2], selectedListing[3], selectedListing[4], selectedListing[5], selectedListing[6], selectedListing[7], selectedListing[8], selectedListing[9], selectedListing[10], selectedListing[11], selectedListing[12])    

    def buyerSearchPropertyListing(self, title, status):
        cursor = mydb.cursor()
        query = "SELECT listing_id, title, img FROM Listings WHERE title LIKE %s AND status=%s AND suspend = 0"
        data = (f"%{title}%",status)
        cursor.execute(query,data)
        listings = cursor.fetchall()
        mydb.commit()
        listingList = []
        if listings:
            for (listing_id, title, img) in listings:
                listing = PropertyListing(listing_id, title, None, img, None,  None, None, None, None, None, None, None, None)
                listingList.append(listing)
        return listingList

    def shortlistProperty(self, user_id, listing_id):

        mycursor = mydb.cursor()

        query1 =  "SELECT * FROM shortlistings WHERE buyer_id=%s AND listing_id=%s"
        data1 = (user_id, listing_id)
        mycursor.execute(query1,data1)

        exist = mycursor.fetchone()

        if(exist):
            query2 =  "UPDATE shortlistings SET shortlist = 1 WHERE buyer_id=%s AND listing_id=%s"
            data2 = (user_id, listing_id)
            mycursor.execute(query2,data2)
        else:
            query2 =  "INSERT INTO shortlistings (buyer_id, listing_id, shortlist) VALUES (%s, %s, 1)"
            data2 = (user_id, listing_id)
            mycursor.execute(query2,data2)
        
        query3 = "UPDATE listings SET shortlist_count = shortlist_count + 1 WHERE listing_id = %s"
        data3 = [listing_id]
        mycursor.execute(query3,data3)
        mydb.commit()

        return True
     
    def unshortlistProperty(self, user_id, listing_id):
        mycursor = mydb.cursor()

        query1 =  "UPDATE shortlistings SET shortlist = 0 WHERE buyer_id=%s AND listing_id=%s"
        data1 = (user_id, listing_id)
        mycursor.execute(query1,data1)

        query2 = "UPDATE listings SET shortlist_count = shortlist_count - 1 WHERE listing_id = %s"
        data2 = [listing_id]
        mycursor.execute(query2,data2)

        mydb.commit()

        return True

    def sellerViewMyPropertyListings(self, seller_id):
        mycursor = mydb.cursor()
        query = "SELECT listing_id, title, img FROM listings where seller_id=%s"
        data =[seller_id]
        mycursor.execute(query,data)

        allListings = mycursor.fetchall()
        mydb.commit()
        listingList = []

        if allListings:
            for (listing_id, title, img) in allListings:
                listing = PropertyListing(listing_id, title, None, img, None, None, None, None, None, None, None, None, None)
                listingList.append(listing)
        return listingList

class Rating:
    def __init__(self, rating_id=None, customer_id=None, agent_id=None, rating=None, customer_profile= None):
        self.rating_id = rating_id
        self.customer_id = customer_id
        self.agent_id = agent_id
        self.rating = rating
        self.customer_profile = customer_profile

    def createRating(self, customer_id, agent_id, rating):
        try:
            mycursor = mydb.cursor()
            query = "INSERT INTO ratings (customer_id, agent_id, rating) VALUES (%s, %s, %s)"
            data = (customer_id, agent_id, rating)
            mycursor.execute(query, data)
            mydb.commit()
            return True
        except Exception as e:
            print(f"Error creating rating: {e}")
            return False  
        
    def viewRatings(self, agent_id):
        mycursor = mydb.cursor()
        query = "SELECT r.rating, u.profileName FROM ratings r Left JOIN user_accounts u ON r.customer_id = u.user_id WHERE r.agent_id = %s"
        data = [agent_id]
        mycursor.execute(query, data)

        ratings = mycursor.fetchall()
    
        mydb.commit()
        ratingList = []
        
        if (ratings):
            for (rating, profileName) in ratings:
                temp = Rating(None, None, None, rating, profileName)
                ratingList.append(temp)
        return ratingList
    
class Review:
    def __init__(self, review_id=None, customer_id=None, agent_id=None, review=None, customer_profile=None):
        self.review_id = review_id
        self.customer_id = customer_id
        self.agent_id = agent_id
        self.review = review
        self.customer_profile = customer_profile

    def createReview(self, customer_id, agent_id, review):
        try:
            mycursor = mydb.cursor()
            query = "INSERT INTO reviews (customer_id, agent_id, review) VALUES (%s, %s, %s)"
            data = (customer_id, agent_id, review)
            mycursor.execute(query, data)
            mydb.commit()
            return True
        except Exception as e:
            print(f"Error creating rating: {e}")
            return False  
        
    def viewReview(self, agent_id):
        mycursor = mydb.cursor()
        query = "SELECT r.review, u.profileName FROM reviews r Left JOIN user_accounts u ON r.customer_id = u.user_id WHERE r.agent_id = %s"
        data = [agent_id]
        mycursor.execute(query, data)

        reviews = mycursor.fetchall()
    
        mydb.commit()
        reviewList = []
        if reviews:
            for (review, profileName) in reviews:
                temp = Review(None, None, None, review, profileName)
                reviewList.append(temp)
        return reviewList
  
        